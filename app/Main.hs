module Main (main) where

import GHC.Utils.Outputable (defaultSDocContext, printSDocLn)
import GHC.Utils.Ppr (Mode (PageMode))
import System.Environment (getArgs)
import System.IO (stdout)
import Test.DocTest.Driver.Extract (dump, extractDocTests)

main :: IO ()
main = do
  dir : opts <- getArgs
  tests <- extractDocTests opts dir
  printSDocLn defaultSDocContext (PageMode False) stdout (dump tests)
