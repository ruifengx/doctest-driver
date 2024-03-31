module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Test.DocTest.Driver.CodeGen (codeGen)
import Test.DocTest.Driver.Extract (extractDocTests)

main :: IO ()
main = do
  rawArgs <- getArgs
  case processArgs rawArgs of
    Nothing   -> hPutStrLn stderr "invalid arguments" *> exitFailure
    Just args -> do
      tests <- concat <$> traverse (extractDocTests args.ghcOptions) args.sourceDirs
      codeGen args.targetDir tests

data Args = Args
  { targetDir  :: FilePath
  , sourceDirs :: [FilePath]
  , ghcOptions :: [String]
  }

processArgs :: [String] -> Maybe Args
processArgs [] = Nothing
processArgs (targetDir : rest)
  | _ : ghcOptions <- opts = Just Args{ targetDir, sourceDirs, ghcOptions }
  | otherwise = Nothing
  where (sourceDirs, opts) = break (== "--") rest
