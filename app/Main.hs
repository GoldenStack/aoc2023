{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isDigit, digitToInt)
import Data.List.Split (splitOn)
import GHC.IO.IOMode (IOMode(ReadMode))

main :: IO ()
main = do
--   let url = "https://adventofcode.com/2023/day/1/input"

--   response <- simpleHttp url
--   let responseBody = L8.unpack response

--   print (day1 $ splitOn "\n" responseBody)
    handle <- readFile "./input/day1.txt"

    print $ day1 $ splitOn "\n" handle


-- day1 :: [String] -> Int
-- day1 input = sum $ map (\l -> head l + last l) $ map filter isDigit input
-- day1 :: [String] 

-- day1 :: [String] -> [Int]
day1 = sum . map (extract . map digitToInt . filter isDigit)
    where extract i = 10 * head i + last i
