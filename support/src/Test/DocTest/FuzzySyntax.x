{
-- | Description: Fuzzy pattern syntax for doctest output.
-- Copyright: Copyright 2024, Ruifeng Xie
-- License: LGPL-3.0-or-later
-- Maintainer: Ruifeng Xie <ruifengx@outlook.com>
--
-- Fuzzy pattern syntax used for doctest output text segments.
module Test.DocTest.FuzzySyntax
  ( Pattern
  , Segment (..)
  , parsePattern
  ) where
}

%wrapper "basic"

tokens :-
  ^ "<BLANKLINE>" $  { const BlankLine }
  ^ "..." $          { const MultilineDots }
  "..."              { const InlineDots }
  .                  { Plain }
  \n                 { Plain }

{
-- | Pattern string.
type Pattern = [Segment]

-- | Pattern segment.
data Segment
  = BlankLine     -- ^ "@\<BLANKLINE\>@" for empty lines.
  | MultilineDots -- ^ "@...@" on its own line, matching one or more arbitrary lines.
  | InlineDots    -- ^ "@...@" in the middle of line, matching one or more non-newline characters.
  | Plain String  -- ^ Other plain string, matched verbatim.
  deriving stock (Eq, Show)

merge :: Monoid b => (a -> Maybe b) -> (b -> a) -> [a] -> [a]
merge _ _ [] = []
merge p m (x : xs)
  | Just y <- p x = m (y <> yt) : merge p m zs
  | otherwise = x : merge p m xs
  where (yt, zs) = mergeWorker p xs

mergeWorker :: Monoid b => (a -> Maybe b) -> [a] -> (b, [a])
mergeWorker _ [] = (mempty, [])
mergeWorker p xs@(x : xs')
  | Just y <- p x = (y <> yt, zs)
  | otherwise = (mempty, xs)
  where (yt, zs) = mergeWorker p xs'

isPlain :: Segment -> Maybe String
isPlain (Plain s) = Just s
isPlain _         = Nothing

mergePlain :: [Segment] -> [Segment]
mergePlain = merge isPlain Plain

-- | Parsing the pattern string into a 'Pattern' structure.
parsePattern :: String -> Pattern
parsePattern = mergePlain . alexScanTokens
}
