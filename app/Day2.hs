{-# LANGUAGE OverloadedStrings #-}
module Day2 where
import Data.String.Conversions (cs)
import Data.Text (Text, splitOn, replace)

-- Parses a game into an ID and a 2d list of rounds.
parseGame text = (gameId, [y | x <- named, y <- x])
  where
    list2round :: [Text] -> (Int, Text)
    list2round (count : name) = (read (cs count) :: Int, head name)
    list2round [] = error "Expected full round data!"

    named = map (map (list2round . splitOn " ")) split
    split = map (splitOn ", ") $ splitOn "; " (init !! 1)

    gameId = read (cs $ replace "Game " "" $ head init) :: Int

    init = splitOn ": " text

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
day2p1 = sum . map fst . filter verify . map parseGame

-- Sum the powers of the games.
day2p2 = sum . map (power . parseGame)
