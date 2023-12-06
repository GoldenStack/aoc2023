{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.String.Conversions (cs)
import Data.Text (Text, length, replace, splitOn)

parseCard text = (id, (winning, have))
  where
    (rawid:rawcards:_) = splitOn ": " text

    id :: Int
    id = read $ cs $ replace "Card " "" rawid

    parse :: Text -> [Int]
    parse line = map (read . cs) $ filter ((> 0) . Data.Text.length) (splitOn " " line)
    
    (winning:have:_) = map parse $ splitOn "|" rawcards

matches (winning, have) = Prelude.length $ filter (`elem` have) winning

wins card = 2 ^ max 0 (matches card - 1)

day4p1 = sum . map (wins . snd . parseCard)

getNext cards (id, info) = filter (<= Prelude.length cards) $ map (+ id) [1 .. matches info]

expand cards indices = [if i `elem` indices then 1 else 0 | i <- [1 .. Prelude.length cards]]

allAdditions cards = expand cards . getNext cards

calc cards = foldl fold ones cards
  where
    ones = replicate (Prelude.length cards) 1
    fold values card = zipWith (+) values (map (* (values !! (fst card -1))) (allAdditions cards card))

day4p2 = sum . calc . map parseCard