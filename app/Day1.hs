{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Control.Monad (foldM)
import Data.Char (digitToInt, isDigit)
import Data.Foldable (minimumBy)
import Data.List.NonEmpty (fromList)
import Data.String (IsString (fromString))
import Data.String.Conversions
import Data.Text (Text, length, replace, splitOn)

-- List of replacements.
num2num = [("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9")]

-- Extract the head and tail from only digits.
count = extract . map digitToInt . filter isDigit
  where
    extract i = 10 * head i + last i

-- Just sum and map by the count.
day1 = sum . map count

-- Consecutively replace.
day1p2 = sum . map (count . cs . replaceOrdered num2num)

-- Replace consecutively
replaceOrdered mappings string = if replaced == string then string else replaceOrdered mappings replaced
    where replaced = replaceFirst mappings string

-- Conducts the first possible replacement, given an array of placement mappings and a string.
replaceFirst mappings string =
  if snd first < Data.Text.length string
    then uncurry replace (fst first) string
    else string
  where
    first = minimumBy (\a b -> compare (snd a) (snd b)) zipped
    zipped = zip mappings (map (index string . fst) mappings)

-- Find the index of the first occurrence of the substring in the text.
index string substring = Data.Text.length $ head split
  where
    split = splitOn substring string