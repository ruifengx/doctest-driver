-- | Description: "Compile-time" utility functions.
-- Copyright: Copyright 2024, Ruifeng Xie
-- License: AGPL-3.0-or-later
-- Maintainer: Ruifeng Xie <ruifengx@outlook.com>
--
-- Utility functions only used by the DocTest-driver itself during code generation. These are not
-- used at runtime by the generated code.
module Test.DocTest.Driver.Utils
  ( splitBy
  , dosLines
  , NaturalOrderString
  , naturalOrdered
  ) where

import Data.Bifunctor (Bifunctor (first))
import Data.Char (digitToInt, isDigit)
import Data.Function (on)
import Data.List (foldl', uncons)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty

-- | Split by the given separator.
--
-- >>> splitBy ' ' "abc def"
-- ["abc","def"]
-- >>> splitBy ' ' "abc  def"
-- ["abc","","def"]
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep xs = s : maybe [] (splitBy sep . snd) (uncons rest)
  where (s, rest) = break (sep ==) xs

-- | 'lines', but with additional support for DOS line endings (CR LF).
dosLines :: String -> [String]
dosLines s  = start : if null rest then [] else dosLines rest
  where (start, rest) = breakDosLine s

breakDosLine :: String -> (String, String)
breakDosLine ""                = ("", "")
breakDosLine "\r"              = ("", "")
breakDosLine ('\n' : s)        = ("", s)
breakDosLine ('\r' : '\n' : s) = ("", s)
breakDosLine (c : s)           = first (c :) (breakDosLine s)

-- | String segment for natural ordering.
newtype NaturalOrderString = NatOrd [Either Integer String]
  deriving stock (Eq, Ord)

-- | Preprocess a string to be compared in the "natural order".
--
-- * Natural order is lexicographical order if there is no digit character:
--
-- >>> naturalOrdered "abc" < naturalOrdered "def"
-- True
--
-- * Natural order considers 2 \< 10, while lexicographical order insists 2 \> 10:
--
-- >>> naturalOrdered "image2" < naturalOrdered "image10"
-- True
--
-- * However, using leading zeros opt into using lexicographical order.
--
-- >>> naturalOrdered "image02" > naturalOrdered "image010"
-- True
-- >>> naturalOrdered "image100" < naturalOrdered "image010"
-- True
naturalOrdered :: String -> NaturalOrderString
naturalOrdered = NatOrd . map go . NonEmpty.groupBy ((==) `on` isDigit)
  where go (x :| xs)
          | x == '0' = if null xs then Left 0 else Right (x : xs)
          | isDigit x = Left (foldl' growInt 0 (x : xs))
          | otherwise = Right (x : xs)
        growInt r y = r * 10 + fromIntegral (digitToInt y)
