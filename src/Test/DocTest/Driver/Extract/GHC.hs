module Test.DocTest.Driver.Extract.GHC
  ( parseModulesIn
  , parseModules
  ) where

import Control.Monad (forM, unless)
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import GHC qualified
import GHC.Paths qualified as GHC

import GHC (Ghc)
import GHC.Data.Graph.Directed (flattenSCCs)
import GHC.Driver.Session (DynFlags (backend, ghcLink, ghcMode), gopt_set)
import GHC.Unit.Module.Graph (filterToposortToModules)
import GHC.Utils.Panic (GhcException (UsageError), throwGhcException)

parseModulesIn :: [String] -> [FilePath] -> IO [GHC.ParsedModule]
parseModulesIn opts dir = do
  paths <- concat <$> traverse recursiveListDirectory dir
  parseModules opts (filter (".hs" `isSuffixOf`) paths)

parseModules :: [String] -> [FilePath] -> IO [GHC.ParsedModule]
parseModules opts paths = GHC.runGhc (Just GHC.libdir) do
  handleOptions opts
  targets <- mapM (\p -> GHC.guessTarget p Nothing Nothing) paths
  GHC.setTargets targets
  modules <- flattenSCCs
    . filterToposortToModules
    . flip (GHC.topSortModuleGraph False) Nothing
    <$> GHC.depanal [] False
  mapM GHC.parseModule modules

handleOptions :: [String] -> Ghc ()
handleOptions opts = do
  logger <- GHC.getLogger
  flags <- setFlags <$> GHC.getSessionDynFlags
  (flags', unknown, _warnings) <- GHC.parseDynamicFlags logger flags (map GHC.noLoc opts)
  unless (null unknown) do
    let msg = "unknown flags: " <> unwords (map show unknown)
    throwGhcException (UsageError msg)
  GHC.setSessionDynFlags flags'

setFlags :: DynFlags -> DynFlags
setFlags flags = (gopt_set flags GHC.Opt_Haddock)
  { backend = GHC.noBackend
  , ghcMode = GHC.CompManager
  , ghcLink = GHC.NoLink
  }

recursiveListDirectory :: FilePath -> IO [FilePath]
recursiveListDirectory dir = do
  children <- map (dir </>) <$> listDirectory dir
  concat <$> forM children \child -> do
    isDir <- doesDirectoryExist child
    if isDir then recursiveListDirectory child else pure [child]
