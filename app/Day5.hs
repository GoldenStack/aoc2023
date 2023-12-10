{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Control.Applicative (Alternative (many))
import Control.Monad (foldM)
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Data.String.Conversions (cs)
import Data.Text (Text, intercalate, length, replace, splitOn)
import Debug.Trace (trace)
import Lib (Parser (run), int, notNull, sepBy, str, word, ws)

data Range = Range
  { dest :: Int,
    source :: Int,
    size :: Int
  }
  deriving (Show)

parseAlmanac = (,) <$> parseSeeds <* ws <*> parseRangeGroups
  where
    parseSeeds = str "seeds:" *> ws *> many (int <* ws)
    parseRange = Range <$> int <* ws <*> int <* ws <*> int
    parseRangeGroups = many ((\_ x -> x) <$> mapName <* ws <*> parseRanges <* ws)
      where
        parseRanges = notNull $ sepBy ws parseRange
        mapName = (,) <$> word <* str "-to-" <*> word <* str " map:" <* ws

mapRange value range
  | inrange = Left $ (value - source range) + dest range
  | otherwise = Right value
  where
    inrange = value >= source range && value < (source range + size range)

passThrough = foldl (\val ranges -> either id id $ mapRanges val ranges)
  where
    mapRanges = foldM mapRange

day5p1 text = do
  almanac <- fmap snd (run parseAlmanac text)

  let seedAndLoc = \seed -> (seed, seed `passThrough` snd almanac)
  let min = minimumBy (comparing snd) $ map seedAndLoc (fst almanac)

  return min

day5p2 = const ""
