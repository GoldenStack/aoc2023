{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.Text (Text, splitOn, empty, length)
import Data.String.Conversions (cs)

parseCard text = if count == 0 then 0 else 2^(count - 1)
  where
    count = Prelude.length both
    both = filter (`elem` have) winning

    winning = parse $ head rest
    have = parse $ rest !! 1

    rest = splitOn "|" $ nums !! 1

    parse :: Text -> [Int]
    parse line = map (read . cs) $ filter ((> 0) . Data.Text.length) (splitOn " " line)
    nums = splitOn ": " text

day4p1 = sum . map parseCard

day4p2 = undefined
