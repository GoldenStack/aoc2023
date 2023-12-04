{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day3 where

import qualified Control.Arrow as Data.Bifunctor
import Data.Char (isDigit)
import Data.List (nub)
import Data.String.Conversions (cs)
import Data.Text (Text, replace, splitOn)

type Board = [[Char]]

width :: Board -> Int
width board = length $ head board

height :: Board -> Int
height = length

-- Provide a safe API for getting from a board
get x y board = if validX && validY then board !! y !! x else '.'
  where
    validX = x >= 0 && x < width board
    validY = y >= 0 && y < height board

-- Figure out if a symbol is a valid part
isSymbol char = char /= '.' && not (isDigit char)

isGear = (==) '*'

-- List of all neighbors
neighbors = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]]

-- Add tuples
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Figure the valid parts that are neighbors
parts :: Int -> Int -> [[Char]] -> [(Int, Int)]
parts x y board = filter (isGear . (\(x2, y2) -> get x2 y2 board)) $ map (add (x, y)) neighbors

-- Figure out if a tile has any parts that are neighbors
-- isPart = (\i -> length i > 0) . parts
isPart x y board = not $ null $ parts x y board

-- Parse an integer value at the given position.
parseValue :: Int -> Int -> Board -> ([Char], [(Int, Int)])
parseValue x y board = if nextIsNum then (get x y board : fst next, part ++ snd next) else ([get x y board], nub part)
  where
    next = parseValue (x + 1) y board
    part = parts x y board
    nextIsNum = isDigit (get (x + 1) y board)

-- Parse a line for all of its numbers
parseLine :: Int -> Int -> Board -> [([Char], [(Int, Int)])]
parseLine x y board = if exit then [] else value
  where
    exit = x >= height board
    isNumber = isDigit $ get x y board
    numberParse = parseValue x y board
    next = x + if isNumber then length (fst numberParse) else 1
    value = if isNumber then numberParse : parseLine next y board else parseLine next y board

-- Parse the entire board for all of its numbers.
parseBoard board = [v | y <- [0 .. height board], v <- parseLine 0 y board]

-- Sum the valid numbers in the board
day3p1 = sum . map (read . fst) . filter (not . null . snd) . parseBoard

-- Sum the valid gears in the board.
day3p2 board = total
  where
    nums = parseBoard board
    expanded = map (\i -> map (fst i,) (snd i)) nums
    flattened = [a | b <- expanded, a <- b]
    keys = nub $ map snd flattened
    reverseMapped = map (\key -> (key, nub $ map fst $ filter (\val -> snd val == key) flattened)) keys
    validGears = filter (\i -> length (snd i) == 2) reverseMapped
    parsedGears = map (Data.Bifunctor.second (map (\v -> read v :: Int))) validGears
    gearValues = map (\(pos, values) -> (pos, (values !! 0 * values !! 1))) parsedGears
    total = sum $ map snd gearValues