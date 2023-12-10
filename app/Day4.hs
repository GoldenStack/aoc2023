{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.String.Conversions (cs)
import Data.Text (Text, length, replace, splitOn)
import Lib (sepBy, str, int, ws, Parser (run), notNull)

optParse = fmap snd . run parseCards

parseCards = notNull $ sepBy (str "\n") parseCard
  where
    parseCard = str "Card" *> ws *> ((,) <$> int <* str ":" <* ws <*> parseSides)
    parseSides = (,) <$> parseNums <* str "|" <*> parseNums
    parseNums = sepBy ws int

matches (winning, have) = Prelude.length $ filter (`elem` have) winning

wins card = 2 ^ max 0 (matches card - 1)

day4p1 = fmap (sum . map (wins . snd)) . optParse

getNext cards (id, info) = filter (<= Prelude.length cards) $ map (+ id) [1 .. matches info]

expand cards indices = [if i `elem` indices then 1 else 0 | i <- [1 .. Prelude.length cards]]

allAdditions cards = expand cards . getNext cards

calc cards = foldl fold ones cards
  where
    ones = replicate (Prelude.length cards) 1
    fold values card = zipWith (+) values (map (* (values !! (fst card -1))) (allAdditions cards card))

day4p2 = fmap (sum . calc) . optParse
