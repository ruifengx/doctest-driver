module Test.DocTest.Driver.Extract
  ( Module (..)
  , DocTests (..)
  , DocLine (..)
  , spanDocLine
  , ExampleLine (..)
  , RichExample (..)
  , ProcessedText (..)
  , Loc
  , extractDocTests
  ) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Exception (assert)
import Data.Bifunctor (second)
import Data.Char (isSpace)
import Data.Foldable (foldMap')
import Data.Function (on, (&))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Generics (everything, mkQ)
import Data.List (intercalate, isPrefixOf, sortBy, sortOn)
import Data.List qualified as List (stripPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (groupBy, head, singleton, toList)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isNothing, mapMaybe)
import Data.Semigroup (Min (Min, getMin))

import GHC
  ( GhcPs
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
import Language.Haskell.Syntax (HsModule (hsmodDecls, hsmodExt))
import Language.Haskell.Syntax.Binds
  ( HsBindLR (FunBind, PatBind, PatSynBind, VarBind, fun_id, var_id)
  , PatSynBind (psb_id)
  , Sig (PatSynSig, TypeSig)
  )
import Language.Haskell.Syntax.Decls
  ( DataDefnCons (NewTypeCon)
  , DocDecl (DocCommentNamed)
  , FamilyDecl (fdInfo, fdLName)
  , FamilyInfo (DataFamily)
  , ForeignDecl (ForeignExport, ForeignImport, fd_name)
  , HsDataDefn (dd_cons)
  , HsDecl (DocD, ForD, SigD, TyClD, ValD)
  , LHsDecl
  , TyClDecl (ClassDecl, DataDecl, FamDecl, SynDecl, tcdDataDefn, tcdFam, tcdLName)
  )
import Language.Haskell.Syntax.Extension (IdP, LIdP)
import Language.Haskell.Syntax.Module.Name (ModuleName (ModuleName))

import Data.Either (partitionEithers)
import Test.DocTest.Driver.Extract.GHC (parseModulesIn)
import Test.DocTest.Driver.Utils (dosLines, naturalOrdered, splitBy)

extractDocTests :: [String] -> [FilePath] -> IO [Module]
extractDocTests opts dirs = map extractFromModule <$> parseModulesIn opts dirs

data Module = Module
  { filePath   :: FilePath
  , modulePath :: [String]
  , importList :: [DocLine]
  , topSetup   :: [DocLine]
  , otherSetup :: [DocLine]
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
  | TestProperty (NonEmpty DocLine)
  | TestExample (NonEmpty ExampleLine)
  | TestExampleRich RichExample
  deriving stock (Show)

data DocLine = DocLine
  { location :: Loc
  , textLine :: String
  } deriving stock (Show)

data ExampleLine = ExampleLine
  { programLine    :: DocLine
  , expectedOutput :: [DocLine]
  } deriving stock (Show)

data RichExample = RichExample
  { programBlock :: NonEmpty DocLine
  , outputText   :: Maybe ProcessedText
  , capturedText :: [ProcessedText]
  } deriving stock (Show)

data ProcessedText = ProcessedText
  { rawTextString :: NonEmpty String
  , identifier    :: Maybe DocLine
  } deriving stock (Show)

extractFromModule :: ParsedModule -> Module
extractFromModule m = Module{ filePath, modulePath, importList, topSetup, otherSetup, testCases }
  where filePath = m.pm_mod_summary.ms_hspp_file
        modulePath = breakModulePath (ms_mod_name m.pm_mod_summary)
        testCases = headerTests <> declTests
        headerTests = m.pm_parsed_source
          & unLoc
          & hsmodExt
          & hsmodHaddockModHeader
          & maybe [] (docToDocTests . unLoc)
        (importList, topSetup, otherSetup, declTests) = m.pm_parsed_source
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
  | Verbatim
  | Blank
  | Other
  deriving stock (Eq, Show)

docLineType :: String -> LineType
docLineType (dropWhile isSpace -> s)
  | null s                 = Blank
  | ">>>"   `isPrefixOf` s = Example
  | ">"     `isPrefixOf` s = Verbatim
  | "prop>" `isPrefixOf` s = Property
  | otherwise              = Other

groupByKey :: (a -> k) -> (k -> k -> Bool) -> [a] -> [(k, NonEmpty (k, a))]
groupByKey key cmp
  = map (fst . NonEmpty.head &&& id)
  . NonEmpty.groupBy (cmp `on` fst)
  . map (key &&& id)

assocLineType :: LineType -> LineType -> Bool
-- properties are always one on each line
assocLineType Property _        = False
assocLineType _        Property = False
-- examples are separated by blank lines
-- multiple example program lines can be grouped together
-- in this case the output will be in the same do block
assocLineType Example  Blank    = False
assocLineType Example  Other    = True
assocLineType Example  Example  = True
-- verbatim code is only joined with more verbatim code
assocLineType Verbatim Verbatim = True
assocLineType Verbatim _        = False
-- now the first one is either blank or other, example can be started
assocLineType _        Example  = False
assocLineType _        Verbatim = False
-- and everything else can be grouped and dropped
assocLineType _        _        = True

data DocGroup
  = GSimple DocTests
  | GVerb GroupKind (NonEmpty DocLine)

data GroupKind
  = GOutput (Maybe DocLine)
  | GInput (Maybe DocLine)
  | GCode DocLine

groupKind :: DocLine -> GroupKind
groupKind s = fromMaybe (GCode s)
  $ GOutput . nullToNothing <$> stripPrefix "output:" s
  <|> GInput . nullToNothing <$> stripPrefix "input:" s
  where nullToNothing t = if null t.textLine then Nothing else Just t

toTestGroup :: (LineType, NonEmpty (LineType, DocLine)) -> Maybe DocGroup
-- properties are one in each group
toTestGroup (Property, (_, p) :| r) = assert (null r) Just (wrapProperty p)
  where wrapProperty = GSimple . TestProperty . NonEmpty.singleton . trimProperty
-- examples are optionally followed by expected output
toTestGroup (Example, exampleLines) = Just (collectExample exampleLines)
  where mkExampleLine (x :| rest) =
          assert (fst x == Example)
          assert (all ((== Other) . fst) rest)
          (expectedOutput, programLine)
          where programLine = trimExample (snd x)
                expectedOutput = unindentLines (map snd rest)
        -- one example optionally followed by several responses
        assocExampleLine l r = l == Example && r == Other
        groupExamples = fromJust . nonEmpty . NonEmpty.groupBy (assocExampleLine `on` fst)
        collectExample = GSimple . TestExample . unindent . fmap mkExampleLine . groupExamples
        -- each example group is unindented separately
        unindent = fmap wrapLine . getCompose . unindentLines . Compose
        wrapLine (expectedOutput, programLine) = ExampleLine{ programLine, expectedOutput }
-- verbatim code are converted to example or property based on inner comment
toTestGroup (Verbatim, verbatimCode) = assert allIsVerbatim (wrap codeLines)
  where allIsVerbatim = all (\(ty, _) -> ty == Verbatim) verbatimCode
        firstLine :| codeLines = unindentLines (fmap (trimVerbatim . snd) verbatimCode)
        comment = trimLeft firstLine
        classify = fmap groupKind . stripPrefix "-- doctest:"
        wrap | Just kind <- classify comment = fmap (GVerb kind) . nonEmpty
             | "-- property:" `isPrefixOf` comment.textLine = fmap (GSimple . TestProperty) . nonEmpty
             | otherwise = const Nothing
-- other lines are simply ignored
toTestGroup _ = Nothing

groupsToCases :: [DocGroup] -> [DocTests]
groupsToCases []                        = []
groupsToCases (GVerb (GCode _) ps : gs) = collectRich ps [] Nothing gs
groupsToCases (GVerb _ _ : gs)          = groupsToCases gs
groupsToCases (GSimple tests : gs)      = tests : groupsToCases gs

collectRich :: NonEmpty DocLine -> [ProcessedText] -> Maybe ProcessedText -> [DocGroup] -> [DocTests]
collectRich code inputs outputText (g : gs)
  | GVerb (GInput identifier) (fmap (.textLine) -> rawTextString) <- g
    = collectRich code (ProcessedText{ identifier, rawTextString } : inputs) outputText gs
  | GVerb (GOutput identifier) (fmap (.textLine) -> rawTextString) <- g
    = assert (isNothing outputText)
      collectRich code inputs (Just ProcessedText{ identifier, rawTextString }) gs
collectRich code inputs outputText gs = TestExampleRich example : groupsToCases gs
  where example = RichExample{ programBlock = code, outputText, capturedText = inputs }

linesToCases :: [DocLine] -> [DocTests]
linesToCases
  = groupsToCases . mapMaybe toTestGroup
  . groupByKey (\l -> docLineType l.textLine) assocLineType

stripPrefix :: String -> DocLine -> Maybe DocLine
stripPrefix p l = DocLine (advanceLoc p l.location) <$> List.stripPrefix p l.textLine

spanDocLine :: (Char -> Bool) -> DocLine -> (String, DocLine)
spanDocLine p l = (left, DocLine (advanceLoc left l.location) rest)
  where (left, rest) = span p l.textLine

-- NOTE: we unindent the lines by only removing extra leading /spaces/.
-- this means other whitespace characters like tabs are not considered at all.
trimLeft :: DocLine -> DocLine
trimLeft = snd . spanDocLine (== ' ')

trimProperty, trimExample :: DocLine -> DocLine
trimProperty = trimPrefix "prop>"
trimExample = trimPrefix ">>>"

trimVerbatim :: DocLine -> DocLine
trimVerbatim = fromJust . stripPrefix ">" . trimLeft

trimPrefix :: String -> DocLine -> DocLine
trimPrefix p = trimLeft . fromJust . stripPrefix p . trimLeft

unindentLines :: Traversable t => t DocLine -> t DocLine
unindentLines theLines = fmap dropSpace theLines
  where -- since all-white lines are excluded, we must not use 'minimum'
        -- because 'minimum' diverges for empty sequences
        level = maybe 0 getMin $ foldMap' (Just . Min) $ Compose (fmap getLevel theLines)
        -- only consider non-white lines for unindent
        getLevel l = if null rest then Nothing else Just (length white)
          where (white, rest) = span (== ' ') l.textLine
        dropSpace l = DocLine (advanceLocBy level l.location) (drop level l.textLine)

docLines :: HsDocString -> [DocLine]
docLines (MultiLineDocString _ lcs) = concatMap docChunkLines lcs
docLines (NestedDocString _ lc)     = docChunkLines lc
docLines (GeneratedDocString c)     = docChunkLines (L generatedSrcSpan c)

docChunkLines :: LHsDocStringChunk -> [DocLine]
docChunkLines (L sp docText) = zipWith DocLine locs (dosLines (unpackHDSC docText))
  where locs = iterate (advanceLoc "\n") (toLoc (srcSpanStart sp))

extractDocs :: [LHsDecl GhcPs] -> ([DocLine], [DocLine], [DocLine], [DocTests])
extractDocs decls = (importList, topSetup, otherSetup, allTests)
  where process (decl, docs) = wrap mainTests
          where name = declName (unLoc decl)
                loc = toLoc (srcSpanStart (getLocA decl))
                wrap tests
                  | null tests = []
                  | Nothing <- name = tests
                  | Just nm <- name = [Group nm loc tests]
                mainTests = concatMap docToDocTests (innerDocs <> docs)
                innerDocs = everything (++) (mkQ [] pure) decl
        groupedDecls = collectDocs (sortBy (leftmost_smallest `on` getLocA) decls)
        -- separate "setup" comments from other ordinary comments
        (topSetupBlocks, setupBlocks, rest) = partition3 go groupedDecls
          where go (L _ (DocD _ (DocCommentNamed name doc)), docs)
                  | "setup:top" `isPrefixOf` name = assert (null docs) C1 ((name, loc), code)
                  | "setup" `isPrefixOf` name = assert (null docs) C2 ((name, loc), code)
                  where loc = toLoc (srcSpanStart (getLoc doc))
                        code = docToDocTests (unLoc doc)
                go decl = C3 decl
        -- sort setup blocks according to their name
        processBlock
          = second (mapMaybe mkGroup . getCompose)
          . traverse processSetup . Compose
          . sortOn (naturalOrdered . fst . fst)
        mkGroup ((name, loc), tests) = if null tests then Nothing else Just (Group name loc tests)
        -- separate "import" declarations from other examples/properties
        ((topImportList, topSetup), topSetupTests) = processBlock topSetupBlocks
        ((otherImportList, otherSetup), otherSetupTests) = processBlock setupBlocks
        importList = sortOn (.textLine) (topImportList <> otherImportList)
        allTests = topSetupTests <> otherSetupTests <> concatMap process rest

processSetup :: [DocTests] -> (([DocLine], [DocLine]), [DocTests])
processSetup = second catMaybes . traverse go
  where go (TestExample ls) = mkExample <$> foldMap keepTests (NonEmpty.toList ls)
        go (TestExampleRich rc)
          | Nothing <- rc.outputText
          , null rc.capturedText
          , program <- NonEmpty.toList rc.programBlock
          = (partitionEithers (map partImport program), Nothing)
        go testCases = pure (Just testCases)
        mkExample = fmap TestExample . nonEmpty
        getImport = fmap trimLeft . stripPrefix "import"
        keepTests l
          | Just modulePath <- getImport l.programLine = (([modulePath], []), [])
          | null l.expectedOutput = (([], [l.programLine]), [])
          | otherwise = (mempty, [l])
        partImport l = maybe (Right l) Left (getImport l)

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
declName (SigD _ sig) = case sig of
  TypeSig   _ names _ -> Just (intercalate ", " (map lIdString names))
  PatSynSig _ names _ -> Just (intercalate ", " (map (\nm -> "pattern " <> lIdString nm) names))
  _                   -> Nothing
declName (ForD _ decl) = case decl of
  ForeignImport{ fd_name } -> Just (lIdString fd_name)
  ForeignExport{ fd_name } -> Just (lIdString fd_name)
declName _ = Nothing

data Sum3 x y z = C1 x | C2 y | C3 z

partition3 :: (p -> Sum3 x y z) -> [p] -> ([x], [y], [z])
partition3 _ []       = ([], [], [])
partition3 f (p : ps) = case f p of
  C1 x -> (x : xs, ys, zs)
  C2 y -> (xs, y : ys, zs)
  C3 z -> (xs, ys, z : zs)
  where (xs, ys, zs) = partition3 f ps
