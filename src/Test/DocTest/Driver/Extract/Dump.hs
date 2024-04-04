module Test.DocTest.Driver.Extract.Dump
  ( Dump (dump)
  , stringDoc
  , printDoc
  , hPrintDoc
  ) where

import Prelude hiding ((<>))

import Test.DocTest.Driver.Extract

import Data.List.NonEmpty (NonEmpty, toList)
import System.IO (Handle, stdout)

import GHC.Types.SrcLoc (RealSrcLoc, srcLocCol, srcLocFile, srcLocLine)
import GHC.Utils.Ppr (Doc, Mode (PageMode), empty, ftext, nest, text, vcat, ($$), (<+>), (<>))
import GHC.Utils.Ppr qualified as P (printDoc, renderStyle, style)

class Dump a where
  dump :: a -> Doc
  dumpList :: [a] -> Doc
  dumpList xs = vcat (map (\x -> text "- " <> nest 2 (dump x)) xs)

hPrintDoc :: Handle -> Doc -> IO ()
hPrintDoc = P.printDoc (PageMode False) 100

printDoc :: Doc -> IO ()
printDoc = hPrintDoc stdout

stringDoc :: Doc -> String
stringDoc = P.renderStyle P.style

dumpTitleList :: Dump a => Bool -> Doc -> [a] -> Doc
dumpTitleList skipEmpty title xs
  | null xs, skipEmpty = empty
  | null xs = title <+> text "[]"
  | otherwise = title $$ dump xs

instance Dump Module where
  dump m = vcat
    [ text "filePath:" <+> dump m.filePath
    , dumpTitleList False (text "modulePath:") m.modulePath
    , dumpTitleList False (text "importList:") m.importList
    , dumpTitleList False (text "topSetup:") m.topSetup
    , dumpTitleList False (text "otherSetup:") m.otherSetup
    , dumpTitleList False (text "testCases:") m.testCases
    ]

instance Dump Int where
  dump = text . show

instance Dump Char where
  dump = text . show
  dumpList = text . show

instance Dump a => Dump [a] where
  dump = dumpList

instance Dump a => Dump (NonEmpty a) where
  dump = dumpList . toList

instance Dump DocLine where
  dump l = dump l.location <> text ": " <> dump l.textLine

instance Dump Loc where
  dump = either ftext dump

instance Dump RealSrcLoc where
  dump l = ftext (srcLocFile l)
    <> text ":" <> dump (srcLocLine l)
    <> text ":" <> dump (srcLocCol l)

instance Dump DocTests where
  dump (Group name loc tests)
    = text "group" <+> dump name <> text "," <+> dump loc <> text ":" $$ dump tests
  dump (TestExample line) = text "example:" $$ dump line
  dump (TestProperty prop) = text "property:" $$ dump prop

instance Dump ExampleLine where
  dump l = text "program:" <+> dump l.programLine
    $$ dumpTitleList True (text "expected:") l.expectedOutput
