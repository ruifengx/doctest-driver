{-# LANGUAGE OverloadedStrings #-}
module Test.DocTest.Driver.CodeGen
  ( Doc
  , runDoc
  , genModuleDoc
  , codeGen
  , codeGenMain
  , codeGenSingle
  ) where

import Test.DocTest.Driver.Extract
  ( DocLine (location, textLine)
  , DocTests (Group, TestExample, TestProperty)
  , ExampleLine (expectedOutput, programLine)
  , Loc
  , Module (importList, modulePath, setupCode, testCases, topSetup)
  , spanDocLine
  )
import Test.DocTest.Driver.Extract.Dump (hPrintDoc)
import Test.DocTest.Driver.Extract.GHC (allowParenthesis, getSessionDynFlags)

import Control.Arrow (Arrow (second), (&&&))
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState (get, put), State, evalState)
import Control.Monad.Writer (MonadWriter (pass, tell), WriterT (WriterT), execWriterT)
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty (groupBy, head)
import Data.Monoid (Ap (Ap))
import Data.String (IsString (fromString))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (WriteMode), withFile)

import GHC (Ghc)
import GHC.Data.FastString (FastString, unpackFS)
import GHC.Driver.Session (DynFlags)
import GHC.Types.SrcLoc (RealSrcLoc, srcLocCol, srcLocFile, srcLocLine)
import GHC.Utils.Ppr qualified as P

newtype MDoc = MDoc P.Doc

instance Semigroup MDoc where
  MDoc l <> MDoc r = MDoc (l P.<> r)

instance Monoid MDoc where
  mempty = MDoc P.empty

type FileLine = (FastString, Int)
type MFileLine = Maybe FileLine

data CodeGenSettings = CGSettings
  { produceLocations :: Bool
  , parserDynFlags   :: DynFlags
  }

type CodeGenM = WriterT MDoc (ReaderT CodeGenSettings (State (Maybe (FastString, Int))))

newtype CodeGen a = Doc (CodeGenM a)
  deriving (Semigroup, Monoid) via Ap CodeGenM a
  deriving
    ( Functor, Applicative, Monad
    , MonadWriter MDoc
    , MonadState MFileLine
    , MonadReader CodeGenSettings
    ) via CodeGenM

type Doc = CodeGen ()

wrapDoc :: P.Doc -> Doc
wrapDoc = Doc . tell . MDoc

runDoc :: CodeGenSettings -> Doc -> P.Doc
runDoc settings (Doc d) = res
  where MDoc res = evalState (runReaderT (execWriterT d) settings) Nothing

instance IsString Doc where
  fromString = text

text :: String -> Doc
text = wrapDoc . P.text

ftext :: FastString -> Doc
ftext = wrapDoc . P.ftext

textShow :: Show a => a -> Doc
textShow = text . show

infixr 6 <+>
(<+>) :: Doc -> Doc -> Doc
Doc l <+> Doc r = Doc (WriterT (liftA2 combine (execWriterT l) (execWriterT r)))
  where combine (MDoc dl) (MDoc dr) = ((), MDoc (dl P.<+> dr))

infixl 5 $$
($$) :: Doc -> Doc -> Doc
Doc l $$ Doc r = Doc $ WriterT do
  MDoc dl <- execWriterT l
  originalLoc <- get
  unless (P.isEmpty dl) (put (fmap (second succ) originalLoc))
  MDoc dr <- execWriterT r
  when (P.isEmpty dr) (put originalLoc)
  pure ((), MDoc (dl P.$$ dr))

vcat :: Foldable f => f Doc -> Doc
vcat = foldr ($$) mempty

emptyText :: Doc
emptyText = wrapDoc P.emptyText

nest :: Int -> Doc -> Doc
nest n = pass . fmap (, coerce (P.nest n))

realLocDoc :: Doc -> RealSrcLoc -> Doc
realLocDoc prefix loc = asks (.produceLocations) >>= \produceLoc ->
  if not produceLoc then prefix else do
  let lineInfo = srcLocFile &&& srcLocLine
  let newInfo@(file, line) = lineInfo loc
  let linePragma = "{-# LINE " <> textShow line <+> textShow file <> " #-}"
  let colPragma = prefix <> "{-# COLUMN " <> textShow (srcLocCol loc) <> " #-}"
  currentLoc <- get
  if currentLoc == Just newInfo then colPragma else linePragma $$ colPragma
  put (Just newInfo)

locDoc :: Doc -> Loc -> Doc
locDoc prefix = either ftext (realLocDoc prefix)

locLine :: Loc -> String
locLine = either unpackFS (\l -> "line " <> show (srcLocLine l))

genModuleDoc :: Module -> Doc
genModuleDoc m = vcat
  [ "module DocTests." <> text modulePath <> " (spec) where"
  , emptyText
  , "import Test.Hspec"
  , "import Test.Hspec.QuickCheck"
  , emptyText
  , "import " <> text modulePath
  , emptyText
  , importList
  , emptyText
  , globalSetup
  , emptyText
  , "spec :: Spec"
  , if null contents then "spec = pure ()" else entry
  ]
  where modulePath = intercalate "." m.modulePath
        importList = vcat (map singleImport m.importList)
        singleImport l = locDoc "import " l.location <> text l.textLine
        contents = map genSetup m.setupCode <> map genDocTests m.testCases
        entry = "spec = describe " <> textShow modulePath <> " $ do" $$ nest 2 (vcat contents)
        globalSetup = vcat (map lineDoc m.topSetup)

lineDocWith :: Doc -> Doc -> DocLine -> Doc
lineDocWith before after l
  = text white <> before <> locDoc mempty real.location <> text real.textLine <> after
  where (white, real) = spanDocLine isSpace l

lineDoc :: DocLine -> Doc
lineDoc = lineDocWith mempty mempty

genSetup :: DocTests -> Doc
genSetup = go []
  where go path (Group name _ tests)       = vcat (map (go (name : path)) tests)
        go path (TestExample exampleLines) = vcat (fmap (genEx path) exampleLines)
        go path (TestProperty propLine)    = wrapPath path (genProperty propLine)
        genEx path l
          | null l.expectedOutput = lineDoc l.programLine
          | otherwise = wrapPath path do
            flags <- asks (.parserDynFlags)
            genExample (allowParenthesis flags l.programLine.textLine) l
        wrapPath path d = foldl (\t p -> "describe " <> textShow p $$ nest 2 t) d path

genDocTests :: DocTests -> Doc
genDocTests (Group name loc tests) = header $$ nest 2 (vcat (map genDocTests tests))
  where header = "describe " <> textShow (name <> groupName) <> " $ do"
        groupName = either (const "") (\l -> " (line " <> show (srcLocLine l) <> ")") loc
genDocTests (TestExample exampleLines) = header $$ nest 2 contents
  where header = "it " <> textShow line <> " $ do"
        line = locLine (exampleLoc (NonEmpty.head exampleLines))
        exampleLoc l = l.programLine.location
        contents = vcat (map genExamples (NonEmpty.groupBy sameGroup exampleLines))
        indentOf l = length (takeWhile isSpace l.programLine.textLine)
        sameGroup l r = indentOf l > indentOf r && null l.expectedOutput
genDocTests (TestProperty propLine) = genProperty propLine

genExamples :: NonEmpty ExampleLine -> Doc
genExamples ls = do
  flags <- asks (.parserDynFlags)
  let ok l = allowParenthesis flags l.programLine.textLine
  vcat (fmap (genExample (all ok ls)) ls)

genExample :: Bool -> ExampleLine -> Doc
genExample allowParen l = line $$ nest 2 (prepend (map lineDoc l.expectedOutput))
  where prepend xs = if null xs then mempty else "`shouldBe`" $$ "(" <> vcat xs <> ")"
        line = if allowParen then lineDocWith "(" ")" l.programLine else lineDoc l.programLine

genProperty :: DocLine -> Doc
genProperty propLine = header $$ nest 2 (lineDoc propLine)
  where header = "prop " <> textShow line <> " $"
        line = locLine propLine.location

genMainDoc :: [Module] -> Doc
genMainDoc ms = vcat
  [ "module Main (main) where"
  , emptyText
  , "import Test.Hspec"
  , emptyText
  , importList
  , emptyText
  , "main :: IO ()"
  , "main = hspec $ do"
  , nest 2 testList
  ]
  where importList = vcat (map ("import qualified " <>) modulePaths)
        testList = vcat (map (<> ".spec") modulePaths)
        modulePaths = map modulePath ms
        modulePath m = "DocTests." <> text (intercalate "." m.modulePath)

writeToFile :: FilePath -> Doc -> Ghc ()
writeToFile path doc = do
  parserDynFlags <- getSessionDynFlags
  let settings = CGSettings{ produceLocations = True, parserDynFlags }
  liftIO do
    createDirectoryIfMissing True (takeDirectory path)
    withFile path WriteMode \hFile -> hPrintDoc hFile (runDoc settings doc)

codeGenSingle :: FilePath -> Module -> Ghc FilePath
codeGenSingle root m = do
  let modulePath = "DocTests" : m.modulePath
  let path = root </> foldr (</>) "" modulePath <> ".hs"
  writeToFile path (genModuleDoc m)
  pure (intercalate "." modulePath)

codeGenMain :: FilePath -> [Module] -> Ghc ()
codeGenMain root ms = writeToFile (root </> "Main.hs") (genMainDoc ms)

codeGen :: FilePath -> [Module] -> Ghc [FilePath]
codeGen root ms = do
  codeGenMain root ms
  traverse (codeGenSingle root) ms
