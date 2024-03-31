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

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep xs = s : maybe [] (splitBy sep . snd) (uncons rest)
  where (s, rest) = break (sep ==) xs

dosLines :: String -> [String]
dosLines "" = []
dosLines s  = start : dosLines rest
  where (start, rest) = breakDosLine s

breakDosLine :: String -> (String, String)
breakDosLine ""                = ("", "")
breakDosLine "\r"              = ("", "")
breakDosLine ('\n' : s)        = ("", s)
breakDosLine ('\r' : '\n' : s) = ("", s)
breakDosLine (c : s)           = first (c :) (breakDosLine s)

newtype NaturalOrderString = NatOrd [Either Integer String]
  deriving stock (Eq, Ord)

naturalOrdered :: String -> NaturalOrderString
naturalOrdered = NatOrd . map go . NonEmpty.groupBy ((==) `on` isDigit)
  where go (x :| xs)
          | x == '0' = if null xs then Left 0 else Right (x : xs)
          | isDigit x = Left (foldl' growInt 0 (x : xs))
          | otherwise = Right (x : xs)
        growInt r y = r * 10 + fromIntegral (digitToInt y)
