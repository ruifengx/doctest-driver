-- | Description: Pretty-printing for extracted information.
-- Copyright: Copyright 2024, Ruifeng Xie
-- License: AGPL-3.0-or-later
-- Maintainer: Ruifeng Xie <ruifengx@outlook.com>
--
-- Pretty-printing for extracted information, intended for debugging purposes.
module Test.DocTest.Driver.Extract.Dump
  ( Dump (..)
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

-- | Pretty-printing for extracted 'Module' etc.
class Dump a where
  -- | Pretty-print to a GHC 'Doc'.
  dump :: a -> Doc
  -- | Pretty-print a list to a GHC 'Doc'.
  dumpList :: [a] -> Doc
  dumpList xs = vcat (map (\x -> text "- " <> nest 2 (dump x)) xs)

-- | Convenience function for print the GHC 'Doc' to a given 'Handle'.
hPrintDoc :: Handle -> Doc -> IO ()
hPrintDoc = P.printDoc (PageMode False) 100

-- | Convenience function for print the GHC 'Doc' to 'stdout'.
printDoc :: Doc -> IO ()
printDoc = hPrintDoc stdout

-- | Convenience function for converting a GHC 'Doc' to a 'String'.
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

instance Dump a => Dump (Maybe a) where
  dump = maybe (text "null") dump

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
  dump (TestExampleRich rich) = text "rich-example:" $$ dump rich

instance Dump ExampleLine where
  dump l = text "program:" <+> dump l.programLine
    $$ dumpTitleList True (text "expected:") l.expectedOutput

instance Dump RichExample where
  dump r = text "programBlock:" <+> dump r.programBlock
    $$ text "outputText:" <+> dump r.outputText
    $$ text "capturedText:" <+> dump r.capturedText

instance Dump ProcessedText where
  dump p = text "rawTextString:" <+> dump p.rawTextString
    $$ text "identifier:" <+> dump p.identifier
