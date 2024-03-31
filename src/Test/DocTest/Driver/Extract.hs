module Test.DocTest.Driver.Extract
  ( Module (..)
  , DocTests (..)
  , DocLine (..)
  , ExampleLine (..)
  , Loc
  , extractDocTests
  ) where

import Control.Arrow ((&&&))
import Control.Exception (assert)
import Control.Monad (filterM)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.Function (on, (&))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Generics (everything, mkQ)
import Data.List (isPrefixOf, sortBy, sortOn)
import Data.List qualified as List (stripPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty (groupBy, head)
import Data.Maybe (fromJust, mapMaybe)

import GHC
  ( GhcPs
  , HsModule (hsmodDecls, hsmodExt)
  , ModSummary (ms_hspp_file)
  , ParsedModule (pm_mod_summary, pm_parsed_source)
  , XModulePs (hsmodHaddockModHeader)
  , getLoc
  , getLocA
  , leftmost_smallest
  , ms_mod_name
  , unLoc
  , unpackHDSC
  )
import GHC.Data.FastString (FastString, unpackFS)
import GHC.Hs.Doc
  ( HsDoc
  , HsDocString (GeneratedDocString, MultiLineDocString, NestedDocString)
  , LHsDocStringChunk
  , hsDocString
  )
import GHC.HsToCore.Docs (collectDocs)
import GHC.Types.Name (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc
  ( GenLocated (L)
  , RealSrcLoc
  , SrcLoc (RealSrcLoc, UnhelpfulLoc)
  , advanceSrcLoc
  , generatedSrcSpan
  , mkRealSrcLoc
  , srcLocCol
  , srcLocFile
  , srcLocLine
  , srcSpanStart
  )
import Language.Haskell.Syntax.Binds
  (HsBindLR (FunBind, PatBind, PatSynBind, VarBind, fun_id, var_id), PatSynBind (psb_id))
import Language.Haskell.Syntax.Decls
  ( DataDefnCons (NewTypeCon)
  , DocDecl (DocCommentNamed)
  , FamilyDecl (fdInfo, fdLName)
  , FamilyInfo (DataFamily)
  , ForeignDecl (ForeignExport, ForeignImport, fd_name)
  , HsDataDefn (dd_cons)
  , HsDecl (DocD, ForD, TyClD, ValD)
  , LHsDecl
  , TyClDecl (ClassDecl, DataDecl, FamDecl, SynDecl, tcdDataDefn, tcdFam, tcdLName)
  )
import Language.Haskell.Syntax.Extension (IdP, LIdP)
import Language.Haskell.Syntax.Module.Name (ModuleName (ModuleName))

import Test.DocTest.Driver.Extract.GHC (parseModulesIn)
import Test.DocTest.Driver.Utils (dosLines, naturalOrdered, splitBy)

extractDocTests :: [String] -> FilePath -> IO [Module]
extractDocTests opts dir = map extractFromModule <$> parseModulesIn opts dir

data Module = Module
  { filePath   :: FilePath
  , modulePath :: [String]
  , importList :: [DocLine]
  , setupCode  :: [DocTests]
  , testCases  :: [DocTests]
  } deriving stock (Show)

type Loc = Either FastString RealSrcLoc

toLoc :: SrcLoc -> Loc
toLoc (RealSrcLoc loc _) = Right loc
toLoc (UnhelpfulLoc msg) = Left msg

advanceLoc :: String -> Loc -> Loc
advanceLoc = fmap . flip (foldr (flip advanceSrcLoc))

advanceLocBy :: Int -> Loc -> Loc
advanceLocBy n = fmap (\loc -> mkRealSrcLoc (srcLocFile loc) (srcLocLine loc) (srcLocCol loc + n))

data DocTests
  = Group String Loc [DocTests]
  | TestProperty DocLine
  | TestExample [ExampleLine]
  deriving stock (Show)

data DocLine = DocLine
  { location :: Loc
  , textLine :: String
  } deriving stock (Show)

data ExampleLine = ExampleLine
  { programLine    :: DocLine
  , expectedOutput :: [DocLine]
  } deriving stock (Show)

extractFromModule :: ParsedModule -> Module
extractFromModule m = Module{ filePath, modulePath, importList, setupCode, testCases }
  where filePath = m.pm_mod_summary.ms_hspp_file
        modulePath = breakModulePath (ms_mod_name m.pm_mod_summary)
        testCases = headerTests <> declTests
        headerTests = m.pm_parsed_source
          & unLoc
          & hsmodExt
          & hsmodHaddockModHeader
          & maybe [] (docToDocTests . unLoc)
        (importList, setupCode, declTests) = m.pm_parsed_source
          & unLoc
          & hsmodDecls
          & extractDocs
        -- TODO: export list

breakModulePath :: ModuleName -> [String]
breakModulePath (ModuleName path) = splitBy '.' (unpackFS path)

docToDocTests :: HsDoc GhcPs -> [DocTests]
docToDocTests = linesToCases . docLines . hsDocString

data LineType
  = Example
  | Property
  | Blank
  | Other
  deriving stock (Eq)

docLineType :: String -> LineType
docLineType (dropWhile isSpace -> s)
  | null s                 = Blank
  | ">>>"   `isPrefixOf` s = Example
  | "prop>" `isPrefixOf` s = Property
  | otherwise              = Other

groupByKey :: (a -> k) -> (k -> k -> Bool) -> [a] -> [(k, NonEmpty (k, a))]
groupByKey key cmp
  = map (fst . NonEmpty.head &&& id)
  . NonEmpty.groupBy (cmp `on` fst)
  . map (key &&& id)

linesToCases :: [DocLine] -> [DocTests]
linesToCases = mapMaybe toTestCase . groupByKey (\l -> docLineType l.textLine) assocLineType
  where -- properties are always one on each line
        assocLineType Property _        = False
        assocLineType _        Property = False
        -- examples are separated by blank lines
        assocLineType Example  Blank    = False
        assocLineType Example  _        = True
        -- now the first one is either blank or other, example can be started
        assocLineType _        Example  = False
        -- and everything else can be grouped and dropped
        assocLineType _        _        = True
        -- properties are one in each group
        toTestCase (Property, (_, p) :| r) = assert (null r) Just (TestProperty (trimProperty p))
        -- examples are optionally followed by expected output
        toTestCase (Example, exampleLines) = Just (collectExample exampleLines)
          where mkExampleLine (x :| rest) =
                  assert (fst x == Example)
                  assert (all ((== Other) . fst) rest)
                  (expectedOutput, programLine)
                  where programLine = trimExample (snd x)
                        expectedOutput = unindentLines (map snd rest)
                -- one example optionally followed by several responses
                assocExampleLine l r = l == Example && r == Other
                groupExamples = NonEmpty.groupBy (assocExampleLine `on` fst)
                collectExample = TestExample . unindent . map mkExampleLine . groupExamples
                -- each example group is unindented separately
                unindent = map (uncurry (flip ExampleLine)) . getCompose . unindentLines . Compose
        -- other lines are simply ignored
        toTestCase _                       = Nothing

stripPrefix :: String -> DocLine -> Maybe DocLine
stripPrefix p l = DocLine (advanceLoc p l.location) <$> List.stripPrefix p l.textLine

-- NOTE: we unindent the lines by only removing extra leading /spaces/.
-- this means other whitespace characters like tabs are not considered at all.
trimLeft :: DocLine -> DocLine
trimLeft l = DocLine (advanceLoc left l.location) rest
  where (left, rest) = span (== ' ') l.textLine

trimProperty, trimExample :: DocLine -> DocLine
trimProperty = trimLeft . fromJust . stripPrefix "prop>" . trimLeft
trimExample = fromJust . stripPrefix ">>>" . trimLeft

unindentLines :: Traversable t => t DocLine -> t DocLine
unindentLines theLines = fmap dropSpace theLines
  where level = minimum (fmap getLevel theLines)
        getLevel l = length (takeWhile (== ' ') l.textLine)
        dropSpace l = DocLine (advanceLocBy level l.location) (drop level l.textLine)

docLines :: HsDocString -> [DocLine]
docLines (MultiLineDocString _ lcs) = concatMap docChunkLines lcs
docLines (NestedDocString _ lc)     = docChunkLines lc
docLines (GeneratedDocString c)     = docChunkLines (L generatedSrcSpan c)

docChunkLines :: LHsDocStringChunk -> [DocLine]
docChunkLines (L sp docText) = zipWith DocLine locs (dosLines (unpackHDSC docText))
  where locs = iterate (advanceLoc "\n") (toLoc (srcSpanStart sp))

extractDocs :: [LHsDecl GhcPs] -> ([DocLine], [DocTests], [DocTests])
extractDocs decls = (sortOn (.textLine) importList, setupBlocks, concatMap process rest)
  where process (decl, docs) = wrap allTests
          where name = declName (unLoc decl)
                loc = toLoc (srcSpanStart (getLocA decl))
                wrap tests
                  | null tests = []
                  | Nothing <- name = tests
                  | Just nm <- name = [Group nm loc tests]
                allTests = concatMap docToDocTests (innerDocs <> docs)
                innerDocs = everything (++) (mkQ [] pure) decl
        groupedDecls = collectDocs (sortBy (leftmost_smallest `on` getLocA) decls)
        -- separate "setup" comments from other ordinary comments
        (setup, rest) = partitionEithers (map go groupedDecls)
          where go (L _ (DocD _ (DocCommentNamed name doc)), docs)
                  | "setup" `isPrefixOf` name = assert (null docs) Left ((name, loc), tests)
                  where loc = toLoc (srcSpanStart (getLoc doc))
                        tests = docToDocTests (unLoc doc)
                go decl = Right decl
        -- sort setup blocks according to their name
        sortedSetup = sortOn (naturalOrdered . fst . fst) setup
        -- separate "import" declarations from other examples/properties
        (importList, restSetup) = traverse processSetup (Compose sortedSetup)
        setupBlocks = mapMaybe mkGroup (getCompose restSetup)
        mkGroup ((name, loc), tests) = if null tests then Nothing else Just (Group name loc tests)

processSetup :: [DocTests] -> ([DocLine], [DocTests])
processSetup = traverse go
  where go (TestExample ls) = TestExample <$> filterM dropImport ls
        go testCases        = pure testCases
        dropImport l = case stripPrefix "import" (trimLeft l.programLine) of
          Just modulePath -> ([trimLeft modulePath], False)
          Nothing         -> ([], True)

lIdString :: LIdP GhcPs -> String
lIdString = idString . unLoc

idString :: IdP GhcPs -> String
idString = occNameString . rdrNameOcc

declName :: HsDecl GhcPs -> Maybe String
declName (TyClD _ decl) = case decl of
  FamDecl{}   -> Just (familyKind decl.tcdFam.fdInfo <> lIdString (fdLName decl.tcdFam))
  SynDecl{}   -> Just ("type " <> lIdString decl.tcdLName)
  DataDecl{}  -> Just (dataKind decl.tcdDataDefn.dd_cons <> lIdString decl.tcdLName)
  ClassDecl{} -> Just ("class " <> lIdString decl.tcdLName)
  where familyKind DataFamily = "data family "
        familyKind _          = "type family "
        dataKind (NewTypeCon _) = "data "
        dataKind _              = "newtype "
declName (ValD _ bind) = case bind of
  FunBind{ fun_id } -> Just (lIdString fun_id)
  VarBind{ var_id } -> Just (idString var_id)
  PatBind{}         -> Just "pattern binding"
  PatSynBind _ pat  -> Just ("pattern " <> lIdString pat.psb_id)
declName (ForD _ decl) = case decl of
  ForeignImport{ fd_name } -> Just (lIdString fd_name)
  ForeignExport{ fd_name } -> Just (lIdString fd_name)
declName _ = Nothing
