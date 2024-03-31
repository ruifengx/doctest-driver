module Main (main) where

import System.Environment (getArgs)

import Test.DocTest.Driver.Extract (extractDocTests)
import Test.DocTest.Driver.Extract.Dump (printDump)

main :: IO ()
main = do
  dir : opts <- getArgs
  tests <- extractDocTests opts dir
  printDump tests
