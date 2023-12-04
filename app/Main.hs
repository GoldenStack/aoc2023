{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.List.Split (splitOn)
import Day1 (day1p2)
import Data.String.Conversions (cs)

main :: IO ()
main = do
    handle <- readFile "./input/day1.txt"

    let lines = splitOn "\n" handle
    let textLines = map cs lines

    print $ day1p2 textLines


