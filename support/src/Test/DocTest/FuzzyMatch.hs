-- | Description: Fuzzy pattern matching for doctest output.
-- Copyright: Copyright 2024, Ruifeng Xie
-- License: LGPL-3.0-or-later
-- Maintainer: Ruifeng Xie <ruifengx@outlook.com>
--
-- Fuzzy matching logic used for doctest output text segments.
module Test.DocTest.FuzzyMatch
  ( match
  ) where

import Data.Foldable (traverse_)
import Data.Functor (void)
import Test.DocTest.FuzzySyntax (Pattern, Segment (..))
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Text.ParserCombinators.ReadP qualified as P

-- | Match the 'String' with the given 'Pattern'.
--
-- * No wildcard:
--
-- >>> let pat = [Plain "some text\nwithout wildcard"]
-- >>> pat `match` "some text\nwithout wildcard"
-- True
--
-- * Inline wildcard:
--
-- >>> let pat = [Plain "some ", InlineDots, Plain " text"]
-- >>> pat `match` "some single-line text"
-- True
-- >>> pat `match` "some\nmultiline\ntext"
-- False
--
-- * Multiline wildcard:
--
-- >>> let pat = [Plain "some\n", MultilineDots, Plain "\ntext"]
-- >>> pat `match` "some\nmultiline\ntext"
-- True
-- >>> pat `match` "some\n\ntext"
-- True
-- >>> pat `match` "some\nmulti\nline\ntext"
-- True
--
-- * Blank line:
--
-- >>> let pat = [Plain "a blank\n", BlankLine, Plain "\nline"]
-- >>> pat `match` "a blank\n\nline"
-- True
-- >>> pat `match` "a blank\n(not)\nline"
-- False
match :: Pattern -> String -> Bool
match syn s
  | [_] <- results = True
  | otherwise = False
  where results = readP_to_S (matcher syn) s

matcher :: Pattern -> ReadP ()
matcher syn = traverse_ syntax syn <* P.optional (P.char '\n') <* P.eof

syntax :: Segment -> ReadP ()
syntax BlankLine     = pure ()
syntax MultilineDots = P.munch ('\n' /=) `skipSepBy1` P.char '\n'
syntax InlineDots    = P.skipMany1 (P.satisfy ('\n' /=))
syntax (Plain s)     = void (P.string s)

skipSepBy1 :: ReadP a -> ReadP sep -> ReadP ()
skipSepBy1 p sep = p *> P.skipMany (sep *> p)
