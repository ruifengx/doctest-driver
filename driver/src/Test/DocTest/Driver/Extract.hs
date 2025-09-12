-- | Description: DocTest extraction.
-- Copyright: Copyright 2024, Ruifeng Xie
-- License: AGPL-3.0-or-later
-- Maintainer: Ruifeng Xie <ruifengx@outlook.com>
--
-- Extracting DocTests from source code parsed using the GHC API.
module Test.DocTest.Driver.Extract
  ( extractDocTests
  , module Test.DocTest.Driver.Extract.Ast
  ) where

import Test.DocTest.Driver.Extract.Ast
import Test.DocTest.Driver.Extract.GHC

-- | Extract a DocTest t'Module' from each 'FilePath', with specified compiler options.
extractDocTests :: [String] -> [FilePath] -> IO [Module]
extractDocTests opts dirs = map extractModule <$> parseModulesIn opts dirs
