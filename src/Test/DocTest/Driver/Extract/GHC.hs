module Test.DocTest.Driver.Extract.GHC
  ( withGhc
  , parseModulesIn
  , parseModules
  , allowParenthesis
  , GHC.getSessionDynFlags
  , liftIO
  ) where

import Control.Monad (forM, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

import GHC qualified
import GHC.Paths qualified as GHC

import GHC (Ghc)
import GHC.Data.FastString (fsLit)
import GHC.Data.Graph.Directed (flattenSCCs)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Session (DynFlags (backend, ghcLink, ghcMode), gopt_set)
import GHC.Parser.Lexer (ParseResult (PFailed, POk), Token (..), initParserState, lexer, unP)
import GHC.Types.SrcLoc (mkRealSrcLoc)
import GHC.Unit.Module.Graph (filterToposortToModules)
import GHC.Utils.Panic (GhcException (UsageError), throwGhcException)

withGhc :: [String] -> Ghc a -> IO a
withGhc opts m = GHC.runGhc (Just GHC.libdir) (handleOptions opts *> m)

parseModulesIn :: [FilePath] -> Ghc [GHC.ParsedModule]
parseModulesIn dir = do
  paths <- concat <$> traverse (liftIO . recursiveListDirectory) dir
  parseModules (filter (".hs" `isSuffixOf`) paths)

parseModules :: [FilePath] -> Ghc [GHC.ParsedModule]
parseModules paths = do
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

allowParenthesis :: DynFlags -> String -> Bool
allowParenthesis flags s = maybe False (all isSafeToken) (toTokens flags s >>= topLevelTokens)

isSafeToken :: Token -> Bool
isSafeToken ITlam               = False -- "\"
isSafeToken ITvbar              = False -- "|"
isSafeToken (ITlarrow _)        = False -- "<-"
isSafeToken (ITrarrow _)        = False -- "->"
isSafeToken (ITdarrow _)        = False -- "=>"
isSafeToken ITlolly             = False -- \multimap
isSafeToken (ITlarrowtail _)    = False -- "-<"
isSafeToken (ITrarrowtail _)    = False -- ">-"
isSafeToken (ITLarrowtail _)    = False -- "-<<"
isSafeToken (ITRarrowtail _)    = False -- ">>-"
isSafeToken ITsemi              = False -- ";"
isSafeToken (ITunknown _)       = False
isSafeToken (ITlineComment _ _) = False
isSafeToken _                   = True

toTokens :: DynFlags -> String -> Maybe [Token]
toTokens dynFlags s = unwrapResult (unP pTokens state)
  where state = initParserState (initParserOpts dynFlags) (stringToStringBuffer s) initLoc
        initLoc = mkRealSrcLoc (fsLit "<unknown>") 1 1
        pTokens = lexer False (cont . GHC.unLoc)
        cont ITeof = pure []
        cont t     = (t :) <$> pTokens
        unwrapResult (POk _ x)   = Just x
        unwrapResult (PFailed _) = Nothing

topLevelTokens :: [Token] -> Maybe [Token]
topLevelTokens = go []
  where go [] [] = Just []
        go _  [] = Nothing -- extra open bracket
        go cs (t : ts)
          | isOpen t = go (t : cs) ts
        go [] (t : ts)
          | isClose t = Nothing -- extra close bracket
          | otherwise = (t :) <$> go [] ts
        go cs0@(c : cs) (t : ts)
          | isClose t, matchDelim c t = go cs ts
          | isClose t = Nothing -- bracket mismatch
          | otherwise = go cs0 ts -- drop tokens inside brackets

isOpen :: Token -> Bool
isOpen ITocurly        = True
isOpen ITvocurly       = True
isOpen ITobrack        = True
isOpen ITopabrack      = True
isOpen IToparen        = True
isOpen IToubxparen     = True
isOpen (IToparenbar _) = True
isOpen _               = False

isClose :: Token -> Bool
isClose ITccurly        = True
isClose ITvccurly       = True
isClose ITcbrack        = True
isClose ITcpabrack      = True
isClose ITcparen        = True
isClose ITcubxparen     = True
isClose (ITcparenbar _) = True
isClose _               = False

matchDelim :: Token -> Token -> Bool
matchDelim ITocurly        ITccurly        = True
matchDelim ITvocurly       ITvccurly       = True
matchDelim ITobrack        ITcbrack        = True
matchDelim ITopabrack      ITcpabrack      = True
matchDelim IToparen        ITcparen        = True
matchDelim IToubxparen     ITcubxparen     = True
matchDelim (IToparenbar _) (ITcparenbar _) = True
matchDelim _               _               = False
