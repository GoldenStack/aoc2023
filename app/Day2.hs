{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day2 where

import Control.Applicative (Alternative (many))
import Data.String.Conversions (cs)
import Data.Text (Text, replace, splitOn)
import GHC.Base (Alternative ((<|>)))
import Lib (Parser (run), int, sepBy, str, word, ws)

parseGames = sepBy (str "\n") parseGame
  where
    parseGame = str "Game" *> ws *> ((,) <$> int <* str ":" <* ws <*> parseMatches)
    parseMatches = sepBy ((str ";" <|> str ",") <* ws) parseMatch
    parseMatch = (,) <$> int <* ws <*> word

-- Gets the biggest color from the given game.
biggest color = foldl max 0 . items . snd
  where
    items = map fst . filter (\a -> color == snd a)

-- Verifies a game (using magic numbers)
verify game =
  biggest "red" game <= 12
    && biggest "green" game <= 13
    && biggest "blue" game <= 14

power game = biggest "red" game * biggest "green" game * biggest "blue" game

-- Count the number of valid games.
day2p1 text = sum . map fst . filter verify . snd <$> run parseGames text

-- Sum the powers of the games.
day2p2 text = sum . map power . snd <$> run parseGames text
