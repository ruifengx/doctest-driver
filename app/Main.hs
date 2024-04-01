module Main (main) where

import Data.Foldable (traverse_)
import System.Environment (getArgs)

import Test.DocTest.Driver.CodeGen (codeGen)
import Test.DocTest.Driver.Extract (extractDocTests)
import Test.DocTest.Driver.Extract.Dump (dump, printDoc)

main :: IO ()
main = do
  rawArgs <- getArgs
  case processArgs rawArgs of
    Left sourceDirs -> do
      tests <- extractDocTests [] sourceDirs
      printDoc (dump tests)
    Right args -> do
      tests <- extractDocTests args.ghcOptions args.sourceDirs
      modulePaths <- codeGen args.targetDir tests
      traverse_ putStrLn modulePaths

data Args = Args
  { targetDir  :: FilePath
  , sourceDirs :: [FilePath]
  , ghcOptions :: [String]
  }

processArgs :: [String] -> Either [FilePath] Args
processArgs [] = Left []
processArgs ("--" : sourceDirs) = Left sourceDirs
processArgs (targetDir : rest) = Right Args{ targetDir, sourceDirs, ghcOptions }
  where (sourceDirs, drop 1 -> ghcOptions) = break (== "--") rest
