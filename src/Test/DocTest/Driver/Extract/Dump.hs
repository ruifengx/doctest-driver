module Test.DocTest.Driver.Extract.Dump
  ( Dump (dump)
  , stringDump
  , printDump
  , hPrintDump
  ) where

import Prelude hiding ((<>))

import Test.DocTest.Driver.Extract

import GHC.Types.SrcLoc (RealSrcLoc, srcLocCol, srcLocFile, srcLocLine)
import GHC.Utils.Outputable
  ( SDoc
  , defaultSDocContext
  , empty
  , ftext
  , nest
  , printSDoc
  , renderWithContext
  , text
  , vcat
  , ($$)
  , (<+>)
  , (<>)
  )
import GHC.Utils.Ppr (Mode (PageMode))
import System.IO (Handle, stdout)

class Dump a where
  dump :: a -> SDoc
  dumpList :: [a] -> SDoc
  dumpList xs = vcat (map (\x -> text "- " <> nest 2 (dump x)) xs)

hPrintDump :: Dump a => Handle -> a -> IO ()
hPrintDump h = printSDoc defaultSDocContext (PageMode False) h . dump

printDump :: Dump a => a -> IO ()
printDump = hPrintDump stdout

stringDump :: Dump a => a -> String
stringDump = renderWithContext defaultSDocContext . dump

dumpTitleList :: Dump a => Bool -> SDoc -> [a] -> SDoc
dumpTitleList skipEmpty title xs
  | null xs, skipEmpty = empty
  | null xs = title <+> text "[]"
  | otherwise = title $$ dump xs

instance Dump Module where
  dump m = vcat
    [ text "filePath:" <+> dump m.filePath
    , dumpTitleList False (text "modulePath:") m.modulePath
    , dumpTitleList False (text "importList:") m.importList
    , dumpTitleList False (text "setupCode:") m.setupCode
    , dumpTitleList False (text "testCases:") m.testCases
    ]

instance Dump Int where
  dump = text . show

instance Dump Char where
  dump = text . show
  dumpList = text . show

instance Dump a => Dump [a] where
  dump = dumpList

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
