{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.List.Split (splitOn)
import Day1 (day1p2)
import Data.String.Conversions (cs)
import Day2 (parseGame, day2p1, day2p2)
import Day3 (parseValue, isPart, neighbors, parseLine, parseBoard, day3p1)
import Data.Char (isDigit)

main :: IO ()
main = do
    handle <- readFile "./input/day3.txt"

    let lines = splitOn "\n" handle
    let textLines = map cs lines

    print $ day3p1 textLines


