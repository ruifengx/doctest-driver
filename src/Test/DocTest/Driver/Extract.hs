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
import Control.Monad (filterM, forM, unless)
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit, isSpace)
import Data.Either (fromRight, partitionEithers)
import Data.Function (on, (&))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Generics (everything, mkQ)
import Data.List (foldl', isPrefixOf, isSuffixOf, sortBy, sortOn, uncons)
import Data.List qualified as List (stripPrefix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty (groupBy, head)
import Data.Maybe (fromJust, mapMaybe)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import GHC (Ghc, GhcPs)
import GHC qualified
import GHC.Data.FastString (FastString, unpackFS)
import GHC.Data.Graph.Directed (flattenSCCs)
import GHC.Driver.Session (DynFlags (backend, ghcLink, ghcMode), gopt_set)
import GHC.Hs.Doc (HsDoc, hsDocString)
import GHC.HsToCore.Docs (collectDocs)
import GHC.Paths qualified as GHC
import GHC.Types.Name (occNameString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc
  (advanceSrcLoc, generatedSrcSpan, mkRealSrcLoc, srcLocCol, srcLocFile, srcLocLine)
import GHC.Unit.Module.Graph (filterToposortToModules)
import GHC.Utils.Panic (GhcException (UsageError), throwGhcException)
import Language.Haskell.Syntax.Binds (HsBindLR (..))
import Language.Haskell.Syntax.Decls
  ( DataDefnCons (..)
  , FamilyDecl (..)
  , FamilyInfo (DataFamily)
  , ForeignDecl (..)
  , HsDecl (..)
  , LHsDecl
  , TyClDecl (..)
  )
import Language.Haskell.Syntax.Extension (IdP, LIdP)
import Language.Haskell.Syntax.Module.Name (ModuleName (ModuleName))

extractDocTests :: [String] -> FilePath -> IO [Module]
extractDocTests opts dir = do
  paths <- filter (".hs" `isSuffixOf`) <$> recursiveListDirectory dir
  map extractFromModule <$> parseModule opts paths

parseModule :: [String] -> [FilePath] -> IO [GHC.ParsedModule]
parseModule opts paths = GHC.runGhc (Just GHC.libdir) do
  handleOptions opts
  targets <- mapM (\p -> GHC.guessTarget p Nothing Nothing) paths
  GHC.setTargets targets
  modules <- flattenSCCs
    . filterToposortToModules
    . flip (GHC.topSortModuleGraph False) Nothing
    <$> GHC.depanal [] False
  mapM GHC.parseModule modules

data Module = Module
  { filePath   :: FilePath
  , modulePath :: [String]
  , importList :: [DocLine]
  , setupCode  :: [DocTests]
  , testCases  :: [DocTests]
  } deriving stock (Show)

type Loc = Either FastString GHC.RealSrcLoc

toLoc :: GHC.SrcLoc -> Loc
toLoc (GHC.RealSrcLoc loc _) = Right loc
toLoc (GHC.UnhelpfulLoc msg) = Left msg

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

extractFromModule :: GHC.ParsedModule -> Module
extractFromModule m = Module{ filePath, modulePath, importList, setupCode, testCases }
  where filePath = m.pm_mod_summary.ms_hspp_file
        modulePath = breakModulePath m.pm_mod_summary.ms_mod.moduleName
        testCases = headerTests <> declTests
        headerTests = m.pm_parsed_source
          & GHC.unLoc
          & GHC.hsmodExt
          & GHC.hsmodHaddockModHeader
          & maybe [] (docToDocTests . GHC.unLoc)
        (importList, setupCode, declTests) = m.pm_parsed_source
          & GHC.unLoc
          & GHC.hsmodDecls
          & extractDocs
        -- TODO: export list

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
                  ExampleLine{ programLine, expectedOutput }
                  where programLine = trimExample (snd x)
                        expectedOutput = unindentLines (map snd rest)
                -- one example optionally followed by several responses
                assocExampleLine l r = l == Example && r == Other
                groupExamples = NonEmpty.groupBy (assocExampleLine `on` fst)
                collectExample = TestExample . map mkExampleLine . groupExamples
        -- other lines are simply ignored
        toTestCase _                       = Nothing

stripPrefix :: String -> DocLine -> Maybe DocLine
stripPrefix p l = DocLine (advanceLoc p l.location) <$> List.stripPrefix p l.textLine

-- NOTE: we unindent the lines by only removing extra leading /spaces/.
-- this means other whitespace characters like tabs are not considered at all.
trimLeft :: DocLine -> DocLine
trimLeft l = DocLine (advanceLoc left l.location) rest
  where (left, rest) = span (== ' ') l.textLine

trimString :: String -> DocLine -> DocLine
trimString p = trimLeft . fromJust . stripPrefix p . trimLeft

trimProperty, trimExample :: DocLine -> DocLine
trimProperty = trimString "prop>"
trimExample = trimString ">>>"

unindentLines :: [DocLine] -> [DocLine]
unindentLines theLines = map dropSpace linesWithLoc
  where level = minimum (map getLevel linesWithLoc)
        linesWithLoc = fromRight (map (0, ) theLines) (traverse extractLoc theLines)
        extractLoc l = fmap (\loc -> (GHC.srcLocCol loc, l)) l.location
        getLevel (loc, l) = length (takeWhile (== ' ') l.textLine) + loc
        dropSpace (loc, l) = DocLine (advanceLocBy level l.location) (drop (level - loc) l.textLine)

docLines :: GHC.HsDocString -> [DocLine]
docLines (GHC.MultiLineDocString _ lcs) = concatMap docChunkLines lcs
docLines (GHC.NestedDocString _ lc)     = docChunkLines lc
docLines (GHC.GeneratedDocString c)     = docChunkLines (GHC.L generatedSrcSpan c)

docChunkLines :: GHC.LHsDocStringChunk -> [DocLine]
docChunkLines (GHC.L sp docText) = zipWith DocLine locs (dosLines (GHC.unpackHDSC docText))
  where locs = iterate (advanceLoc "\n") (toLoc (GHC.srcSpanStart sp))

extractDocs :: [LHsDecl GhcPs] -> ([DocLine], [DocTests], [DocTests])
extractDocs decls = (sortOn (.textLine) importList, setupBlocks, concatMap process rest)
  where process (decl, docs) = wrap allTests
          where name = declName (GHC.unLoc decl)
                loc = toLoc (GHC.srcSpanStart (GHC.getLocA decl))
                wrap tests
                  | null tests = []
                  | Nothing <- name = tests
                  | Just nm <- name = [Group nm loc tests]
                allTests = concatMap docToDocTests (innerDocs <> docs)
                innerDocs = everything (++) (mkQ [] pure) decl
        groupedDecls = collectDocs (sortBy (GHC.leftmost_smallest `on` GHC.getLocA) decls)
        -- separate "setup" comments from other ordinary comments
        (setup, rest) = partitionEithers (map go groupedDecls)
          where go (GHC.L _ (DocD _ (GHC.DocCommentNamed name doc)), docs)
                  | "setup" `isPrefixOf` name = assert (null docs) Left ((name, loc), tests)
                  where loc = toLoc (GHC.srcSpanStart (GHC.getLoc doc))
                        tests = docToDocTests (GHC.unLoc doc)
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
lIdString = idString . GHC.unLoc

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

breakModulePath :: ModuleName -> [String]
breakModulePath (ModuleName path) = splitBy '.' (unpackFS path)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep xs = s : maybe [] (splitBy sep . snd) (uncons rest)
  where (s, rest) = break (sep ==) xs

handleOptions :: [String] -> Ghc ()
handleOptions opts = do
  logger <- GHC.getLogger
  flags <- setFlags <$> GHC.getSessionDynFlags
  (flags', unknown, _warnings) <- GHC.parseDynamicFlags logger flags (map GHC.noLoc opts)
  unless (null unknown) do
    let msg = "unknown flags: " <> unwords (map show unknown)
    throwGhcException (UsageError msg)
  GHC.setSessionDynFlags flags'

setFlags :: DynFlags -> DynFlags
setFlags flags = (gopt_set flags GHC.Opt_Haddock)
  { backend = GHC.noBackend
  , ghcMode = GHC.CompManager
  , ghcLink = GHC.NoLink
  }

recursiveListDirectory :: FilePath -> IO [FilePath]
recursiveListDirectory dir = do
  children <- map (dir </>) <$> listDirectory dir
  concat <$> forM children \child -> do
    isDir <- doesDirectoryExist child
    if isDir then recursiveListDirectory child else pure [child]

dosLines :: String -> [String]
dosLines "" = []
dosLines s  = start : dosLines rest
  where (start, rest) = breakDosLine s

breakDosLine :: String -> (String, String)
breakDosLine ""                = ("", "")
breakDosLine "\r"              = ("", "")
breakDosLine ('\n' : s)        = ("", s)
breakDosLine ('\r' : '\n' : s) = ("", s)
breakDosLine (c : s)           = first (c :) (breakDosLine s)

newtype NaturalOrderString = NatOrd [Either Integer String]
  deriving stock (Eq, Ord)

naturalOrdered :: String -> NaturalOrderString
naturalOrdered = NatOrd . map go . NonEmpty.groupBy ((==) `on` isDigit)
  where go (x :| xs)
          | x == '0' = if null xs then Left 0 else Right (x : xs)
          | isDigit x = Left (foldl' growInt 0 (x : xs))
          | otherwise = Right (x : xs)
        growInt r y = r * 10 + fromIntegral (digitToInt y)
