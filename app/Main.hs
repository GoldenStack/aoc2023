{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.List.Split (splitOn)
import Day1 (day1p2)
import Data.String.Conversions (cs)
import Day2 (parseGame, day2p1, day2p2)

main :: IO ()
main = do
    handle <- readFile "./input/day2.txt"

    let lines = splitOn "\n" handle
    let textLines = map cs lines

    print $ day2p2 textLines


