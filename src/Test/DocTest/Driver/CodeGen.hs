{-# LANGUAGE OverloadedStrings #-}
module Test.DocTest.Driver.CodeGen
  ( Doc
  , runDoc
  , genModuleDoc
  , codeGen
  , codeGenSingle
  ) where

import Test.DocTest.Driver.Extract
  ( DocLine (location, textLine)
  , DocTests (Group, TestExample, TestProperty)
  , ExampleLine (expectedOutput, programLine)
  , Loc
  , Module (importList, modulePath, setupCode, testCases)
  )
import Test.DocTest.Driver.Extract.Dump (hPrintDoc)

import Control.Arrow (Arrow (second), (&&&))
import Control.Monad (unless, when)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Control.Monad.State (MonadState (get, put), State, evalState)
import Control.Monad.Writer (MonadWriter (pass, tell), WriterT (WriterT), execWriterT)
import Data.Coerce (coerce)
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NonEmpty (head)
import Data.Monoid (Ap (Ap))
import Data.String (IsString (fromString))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (WriteMode), withFile)

import GHC.Data.FastString (FastString, unpackFS)
import GHC.Types.SrcLoc (RealSrcLoc, srcLocCol, srcLocFile, srcLocLine)
import GHC.Utils.Ppr qualified as P

newtype MDoc = MDoc P.Doc

instance Semigroup MDoc where
  MDoc l <> MDoc r = MDoc (l P.<> r)

instance Monoid MDoc where
  mempty = MDoc P.empty

type FileLine = (FastString, Int)
type MFileLine = Maybe FileLine

type CodeGenM = WriterT MDoc (ReaderT Bool (State (Maybe (FastString, Int))))

newtype CodeGen a = Doc (CodeGenM a)
  deriving (Semigroup, Monoid) via Ap CodeGenM a
  deriving (Functor, Applicative, Monad, MonadWriter MDoc, MonadState MFileLine, MonadReader Bool) via CodeGenM

type Doc = CodeGen ()

wrapDoc :: P.Doc -> Doc
wrapDoc = Doc . tell . MDoc

runDoc :: Bool -> Doc -> P.Doc
runDoc produceLoc (Doc d) = res
  where MDoc res = evalState (runReaderT (execWriterT d) produceLoc) Nothing

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
realLocDoc prefix loc = ask >>= \produceLoc ->
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
  , "spec :: Spec"
  , "spec = describe " <> textShow modulePath <> " $ do"
  , nest 2 contents
  ]
  where modulePath = intercalate "." m.modulePath
        importList = vcat (map singleImport m.importList)
        singleImport l = locDoc "import " l.location <> text l.textLine
        contents = vcat (map genSetup m.setupCode <> map genDocTests m.testCases)

lineDoc :: DocLine -> Doc
lineDoc l = locDoc mempty l.location <> text l.textLine

genSetup :: DocTests -> Doc
genSetup = go []
  where go path (Group name _ tests)       = vcat (map (go (name : path)) tests)
        go path (TestExample exampleLines) = vcat (fmap (genEx path) exampleLines)
        go path (TestProperty propLine)    = wrapPath path (genProperty propLine)
        genEx path l
          | null l.expectedOutput = lineDoc l.programLine
          | otherwise = wrapPath path (genExample l)
        wrapPath path d = foldl (\t p -> "describe " <> textShow p $$ nest 2 t) d path

genDocTests :: DocTests -> Doc
genDocTests (Group name loc tests) = header $$ nest 2 (vcat (map genDocTests tests))
  where header = "describe " <> textShow (name <> groupName) <> " $ do"
        groupName = either (const "") (\l -> " (line " <> show (srcLocLine l) <> ")") loc
genDocTests (TestExample exampleLines) = header $$ nest 2 contents
  where header = "it " <> textShow line <> " $ do"
        line = locLine (exampleLoc (NonEmpty.head exampleLines))
        exampleLoc l = l.programLine.location
        contents = vcat (fmap genExample exampleLines)
genDocTests (TestProperty propLine) = genProperty propLine

genExample :: ExampleLine -> Doc
genExample l = lineDoc l.programLine $$ nest 2 (vcat (prepend (map lineDoc l.expectedOutput)))
  where prepend xs = if null xs then xs else "`shouldBe`" : xs

genProperty :: DocLine -> Doc
genProperty propLine = header $$ nest 2 (lineDoc propLine)
  where header = "prop " <> textShow line <> " $"
        line = locLine propLine.location

genMainDoc :: [Module] -> Doc
genMainDoc ms = vcat
  [ "module Main (main) where"
  , importList
  , "main :: IO ()"
  , "main = hspec $ do"
  , nest 2 testList
  ]
  where importList = vcat (map ("import qualified " <>) modulePaths)
        testList = vcat (map (<> ".spec") modulePaths)
        modulePaths = map modulePath ms
        modulePath m = "DocTests." <> text (intercalate "." m.modulePath)

writeToFile :: FilePath -> Doc -> IO ()
writeToFile path doc = do
  createDirectoryIfMissing True (takeDirectory path)
  withFile path WriteMode \hFile -> hPrintDoc hFile (runDoc True doc)

codeGenSingle :: FilePath -> Module -> IO FilePath
codeGenSingle root m = do
  let modulePath = "DocTests" : m.modulePath
  let path = root </> foldr (</>) "" modulePath <> ".hs"
  writeToFile path (genModuleDoc m)
  pure (intercalate "." modulePath)

codeGenMain :: FilePath -> [Module] -> IO ()
codeGenMain root ms = writeToFile (root </> "Main.hs") (genMainDoc ms)

codeGen :: FilePath -> [Module] -> IO [FilePath]
codeGen root ms = do
  codeGenMain root ms
  traverse (codeGenSingle root) ms
