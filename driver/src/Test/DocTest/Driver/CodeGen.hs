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
  , DocTests (Group, TestExample, TestExampleRich, TestProperty)
  , ExampleLine (expectedOutput, programLine)
  , Loc
  , Module (importList, modulePath, otherSetup, testCases, topSetup)
  , ProcessedText (identifier, rawTextString)
  , RichExample (RichExample, capturedText, outputText, programBlock)
  , spanDocLine
  )
import Test.DocTest.Driver.Extract.Dump (hPrintDoc)

import Control.Arrow (Arrow (second), (&&&))
import Control.Monad (unless, when)
import Control.Monad.State (MonadState (get, put), State, evalState)
import Control.Monad.Writer (MonadWriter (pass, tell), WriterT (WriterT), execWriterT)
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty (head, toList)
import Data.Maybe (fromJust)
import Data.Monoid (Ap (Ap))
import Data.String (IsString (fromString))
import GHC.Show (showLitString)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (WriteMode), withFile)

import GHC.Data.FastString (FastString, unpackFS)
import GHC.Types.SrcLoc (RealSrcLoc, srcLocCol, srcLocFile, srcLocLine)
import GHC.Utils.Ppr qualified as P

import Data.Version (showVersion)
import PackageInfo_doctest_driver qualified as Info

newtype MDoc = MDoc P.Doc

instance Semigroup MDoc where
  MDoc l <> MDoc r = MDoc (l P.<> r)

instance Monoid MDoc where
  mempty = MDoc P.empty

type FileLine = (FastString, Int)
type MFileLine = Maybe FileLine

type CodeGenM = WriterT MDoc (State (Maybe (FastString, Int)))

newtype CodeGen a = Doc (CodeGenM a)
  deriving (Semigroup, Monoid) via Ap CodeGenM a
  deriving (Functor, Applicative, Monad, MonadWriter MDoc, MonadState MFileLine) via CodeGenM

type Doc = CodeGen ()

wrapDoc :: P.Doc -> Doc
wrapDoc = Doc . tell . MDoc

runDoc :: Doc -> P.Doc
runDoc (Doc d) = res
  where MDoc res = evalState (execWriterT d) Nothing

instance IsString Doc where
  fromString = text

text :: String -> Doc
text = wrapDoc . P.text

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
realLocDoc prefix loc = do
  let lineInfo = srcLocFile &&& srcLocLine
  let newInfo@(file, line) = lineInfo loc
  let linePragma = "{-# LINE " <> textShow line <+> textShow file <> " #-}"
  let colPragma = prefix <> "{-# COLUMN " <> textShow (srcLocCol loc) <> " #-}"
  currentLoc <- get
  if currentLoc == Just newInfo then colPragma else linePragma $$ colPragma
  put (Just newInfo)

locDoc :: Doc -> Loc -> Doc
locDoc prefix = either (const prefix) (realLocDoc prefix)

locLine :: Loc -> String
locLine = either unpackFS (\l -> "line " <> show (srcLocLine l))

noteGenerated :: Doc
noteGenerated = "-- autogenerated by "
  <> text Info.name <+> text (showVersion Info.version)
  <> ", edits will be overwritten upon next build"

genModuleDoc :: Module -> Doc
genModuleDoc m = vcat
  [ noteGenerated
  -- this is unfortunate, but we must import the target module unconditionally, and when the
  -- doctests do not use anything from that module (e.g., when there is no doctest at all), the
  -- import becomes redundant and triggers a warning
  , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
  , "module DocTests." <> text modulePath <> " (spec) where"
  , emptyText
  , "import Test.Hspec"
  , "import Test.Hspec.QuickCheck"
  , emptyText
  , "import " <> text modulePath
  , emptyText
  , vcat (map genImport m.importList)
  , emptyText
  , vcat (map lineDoc m.topSetup)
  , emptyText
  , "spec :: Spec"
  , if null contents then "spec = pure ()" else entry
  ]
  where modulePath = intercalate "." m.modulePath
        contents = map lineDoc m.otherSetup <> map genDocTests m.testCases
        entry = "spec = describe " <> textShow modulePath <> " $ do" $$ nest 2 (vcat contents)

lineDoc :: DocLine -> Doc
lineDoc l = text white <> locDoc mempty real.location <> text real.textLine
  where (white, real) = spanDocLine isSpace l

genImport :: DocLine -> Doc
genImport l = locDoc "import " l.location <> text l.textLine

genDocTests :: DocTests -> Doc
genDocTests (Group name loc tests) = header $$ nest 2 (vcat (map genDocTests tests))
  where header = "describe " <> textShow (name <> groupName) <> " $ do"
        groupName = either (const "") (\l -> " (line " <> show (srcLocLine l) <> ")") loc
genDocTests (TestExample exampleLines) = header $$ nest 2 contents
  where header = if nTests == 1 then "do" else key <+> label <> " $ do"
        key = if nTests == 0 then "it" else "describe"
        nTests = testCount exampleLines
        -- nTests == 0: use "it", the whole group is a test, but no "it" for example lines
        -- nTests == 1: use "do", no need to introduce the group layer for a single test
        -- nTests == 2: use "describe", we need a group only in this case
        label = textShow ("example group (" <> locLine loc <> ")")
        loc = let l = NonEmpty.head exampleLines in l.programLine.location
        contents = vcat (fmap genExample exampleLines)
genDocTests (TestProperty propLine) = genProperty propLine
genDocTests (TestExampleRich richExample) = genExampleRich richExample

genExample :: ExampleLine -> Doc
genExample l
  | null l.expectedOutput = program
  | otherwise = header $$ nest 2 ("(" <> program <> ")" $$ nest 2 ("`shouldBe`" $$ expected))
  where header = "it " <> label <> " $ do"
        label = textShow ("example (" <> locLine l.programLine.location <> ")")
        program = lineDoc l.programLine
        expected = "(" <> vcat (map lineDoc l.expectedOutput) <> ")"

testCount :: NonEmpty ExampleLine -> Int
testCount = go 0 . NonEmpty.toList
  where go n [] = n
        go n (line : rest)
          | null line.expectedOutput = go n rest
          | n == 1 = 2
          | otherwise = go (succ n) rest

genProperty :: NonEmpty DocLine -> Doc
genProperty propLines = header $$ nest 2 (vcat (fmap lineDoc propLines))
  where header = "prop " <> textShow line <> " $"
        line = "property (" <> locLine ((.location) (NonEmpty.head propLines)) <> ")"

genExampleRich :: RichExample -> Doc
genExampleRich RichExample{ programBlock, capturedText, outputText }
  = header $$ nest 2 (bindings $$ program $$ expected)
  where genBinding p = binder (fromJust p.identifier)
          <+> text "=" <+> multilineString p.rawTextString
        binder l = locDoc "let " l.location <> text l.textLine
        bindings = vcat (map genBinding capturedText)
        program = vcat (fmap lineDoc programBlock)
        expected = maybe mempty go outputText
          where go t = nest 2 $ "`shouldBe`"
                  $$ maybe mempty lineDoc t.identifier
                  $$ multilineString t.rawTextString
        header = "it " <> textShow ("multiline example (" <> locLine loc <> ")") <> " $ do"
        loc = let firstLine = NonEmpty.head programBlock in firstLine.location

multilineString :: NonEmpty String -> Doc
multilineString (l :| ls) = if null ls then textShow l else firstLine $$ go ls
  where go []       = error "impossible"
        go [x]      = text ('\\' : showLitString x "\"")
        go (x : xs) = text ('\\' : showLitString x "\\n\\") $$ go xs
        firstLine = text ('"' : showLitString l "\\n\\")

genMainDoc :: [Module] -> Doc
genMainDoc ms = vcat
  [ noteGenerated
  , "module Main (main) where"
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

writeToFile :: FilePath -> Doc -> IO ()
writeToFile path doc = do
  createDirectoryIfMissing True (takeDirectory path)
  withFile path WriteMode \hFile -> hPrintDoc hFile (runDoc doc)

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