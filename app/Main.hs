{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.String.Conversions (cs)
import Day1 (day1p1, day1p2)
import Day2 (day2p1, day2p2)
import Day3 (day3p1, day3p2)
import Day4 (day4p1, day4p2)
import Day5 (day5p1, day5p2)

main :: IO ()
main = do
  handle <- readFile "./input/day5.txt"

  let textLines = cs handle

  print $ day5p1 textLines
  print $ day5p2 textLines
