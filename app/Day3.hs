{-# LANGUAGE OverloadedStrings #-}
module Day3 where
import Data.String.Conversions (cs)
import Data.Text (Text, splitOn, replace)
import Data.Char (isDigit)

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

-- List of all neighbors
neighbors = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]]
-- Add tuples
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2);
-- Figure out if a tile has any parts that are neighbors
isPart x y board = any ((isSymbol . (\(x2, y2) -> get x2 y2 board)) . add (x, y)) neighbors

-- Parse an integer value at the given position.
parseValue :: Int -> Int -> Board -> ([Char], Bool)
parseValue x y board = if nextIsNum then (get x y board:(fst next), part || snd next) else ([get x y board], part)
    where
        next = parseValue (x+1) y board
        part = isPart x y board
        nextIsNum = isDigit (get (x+1) y board)

-- Parse a line for all of its numbers
parseLine :: Int -> Int -> Board -> [([Char], Bool)]
parseLine x y board = if exit then [] else value
    where
        exit = x >= height board
        isNumber = isDigit $ get x y board
        numberParse = parseValue x y board
        next = x + if isNumber then length (fst numberParse) else 1
        value = if isNumber then numberParse:parseLine next y board else parseLine next y board

-- Parse the entire board for all of its numbers.
parseBoard board = [v | y <- [0..height board], v <- parseLine 0 y board]

-- Sum the valid numbers in the board
day3p1 = sum . map (read . fst) . filter snd . parseBoard