{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.String.Conversions (cs)
import Day1 (day1p1, day1p2)
import Day2 (day2p1, day2p2)
import Day3 (day3p1, day3p2)

main :: IO ()
main = do
  handle <- readFile "./input/day3.txt"

  let lines = splitOn "\n" handle
  let textLines = map cs lines

  print $ day3p2 textLines
