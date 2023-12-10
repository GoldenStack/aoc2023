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

parseRangeGroups = many ((\_ x -> x) <$> mapName <* ws <*> parseRanges <* ws)
  where
    parseRanges = notNull $ sepBy ws parseRange
    parseRange = Range <$> int <* ws <*> int <* ws <*> int
    mapName = (,) <$> word <* str "-to-" <*> word <* str " map:" <* ws

parseAlmanac seedParser = (,) <$> (str "seeds:" *> ws *> many (seedParser <* ws)) <* ws <*> parseRangeGroups

parseAlmanac1 = parseAlmanac int

parseAlmanac2 = parseAlmanac $ (,) <$> int <* ws <*> int

mapRange value range
  | inrange = Left $ (value - source range) + dest range
  | otherwise = Right value
  where
    inrange = value >= source range && value < (source range + size range)

passThrough = foldl (\val ranges -> either id id $ mapRanges val ranges)
  where
    mapRanges = foldM mapRange

day5p1 text = do
  almanac <- snd <$> run parseAlmanac1 text

  let seedAndLoc = \seed -> seed `passThrough` snd almanac

  return $ minimum $ map seedAndLoc (fst almanac)

day5p2 text = do
  almanac <- snd <$> run parseAlmanac2 text



  return $ fst almanac
