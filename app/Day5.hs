{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Control.Applicative (Alternative (many))
import Control.Monad (foldM)
import Data.Foldable (minimumBy)
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

data Almanac = Almanac
  { seeds :: [Int],
    mappings :: [[Range]]
  }
  deriving (Show)

parseAlmanac = Almanac <$> parseSeeds <* ws <*> parseRangeGroups
  where
    parseSeeds = str "seeds:" *> ws *> many (int <* ws)
    parseRange = Range <$> int <* ws <*> int <* ws <*> int
    parseRangeGroups = many ((\_ x -> x) <$> mapName <* ws <*> parseRanges <* ws)
      where
        parseRanges = notNull $ sepBy ws parseRange
        mapName = (,) <$> word <* str "-to-" <*> word <* str " map:" <* ws

mapRange :: Int -> Range -> Either Int Int
mapRange value range
  | inrange = Left $ (value - source range) + dest range
  | otherwise = Right value
  where
    inrange = value >= source range && value < (source range + size range)

mapRanges :: Int -> [Range] -> Either Int Int
mapRanges = foldM mapRange

passThrough :: Int -> [[Range]] -> Int
passThrough = foldl (\val ranges -> either id id $ mapRanges val ranges)

day5p1 text = do
  almanac <- fmap snd (run parseAlmanac text)

  let min = minimumBy (\a b -> compare (snd a) (snd b)) $ zip (seeds almanac) $ map (\a -> passThrough a (mappings almanac)) (seeds almanac)

  return min

day5p2 = const ""
