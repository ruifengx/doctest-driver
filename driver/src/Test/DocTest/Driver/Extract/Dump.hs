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
    , text "modulePath:" $$ dump m.modulePath
    , dumpTitleList False (text "importList:") m.importList
    , dumpTitleList False (text "topSetup:") m.topSetup
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
  dump (TestMultiline content) = text "multiline:" $$ dump content
  dump (Capture content body) = text "capture:" $$ nest 2 (dump content $$ text "body:" $$ dump body)
  dump (TestHook hook body) = text "hook:" $$ nest 2 (dump hook $$ text "body:" $$ dump body)
  dump (Warning loc msg) = text "warning (" <> dump loc <> text "): " <> dump msg

instance Dump ExampleLine where
  dump l = text "program:" <+> dump l.programLine
    $$ dumpTitleList True (text "expected:") l.expectedOutput

instance Dump Entity where
  dump = text . show

instance Dump CapturedContent where
  dump c = text "variableName:" <+> dump c.variableName
    $$ text "captureMethod:" <+> dump c.captureMethod
    $$ text "textContent:" <+> dump c.variableName

instance Dump CaptureMethod where
  dump = text . show

instance Dump IOHook where
  dump hook = text "flavour:" <+> dump hook.flavour
    $$ text "variables:" $$ dump hook.variables
    $$ text "setupCode:" $$ dump hook.setupCode

instance Dump HookFlavour where
  dump = text . show
