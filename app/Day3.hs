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
parts :: (Char -> Bool) -> Int -> Int -> [[Char]] -> [(Int, Int)]
parts predicate x y board = filter (predicate . (\(x2, y2) -> get x2 y2 board)) $ map (add (x, y)) neighbors

-- Figure out if a tile has any parts that are neighbors
isPart predicate x y board = not $ null $ parts predicate x y board

-- Parse an integer value at the given position.
parseValue :: (Char -> Bool) -> Int -> Int -> Board -> ([Char], [(Int, Int)])
parseValue predicate x y board = if nextIsNum then (get x y board : fst next, part ++ snd next) else ([get x y board], nub part)
  where
    next = parseValue predicate (x + 1) y board
    part = parts predicate x y board
    nextIsNum = isDigit (get (x + 1) y board)

-- Parse a line for all of its numbers
parseLine :: (Char -> Bool) -> Int -> Int -> Board -> [([Char], [(Int, Int)])]
parseLine predicate x y board = if x >= height board then [] else value
  where
    isNumber = isDigit $ get x y board
    numberParse = parseValue predicate x y board
    xOffset = if isNumber then length (fst numberParse) else 1
    next = parseLine predicate (x + xOffset) y board
    value = if isNumber then numberParse : next else next

-- Parse the entire board for all of its numbers.
parseBoard predicate board = [v | y <- [0 .. height board], v <- parseLine predicate 0 y board]

-- Sum the valid numbers in the board
day3p1 = sum . map (read . fst) . filter (not . null . snd) . parseBoard isSymbol

-- Sum the valid gears in the board.
day3p2 board = total
  where
    nums = parseBoard isGear board
    expanded = map (\i -> map (fst i,) (snd i)) nums
    flattened = [a | b <- expanded, a <- b]
    keys = nub $ map snd flattened
    reverseMapped = map (\key -> (key, nub $ map fst $ filter (\val -> snd val == key) flattened)) keys
    validGears = filter (\i -> length (snd i) == 2) reverseMapped
    parsedGears = map (Data.Bifunctor.second (map (\v -> read v :: Int))) validGears
    gearValues = map (\(pos, values) -> (pos, (values !! 0 * values !! 1))) parsedGears
    total = sum $ map snd gearValues