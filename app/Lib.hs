module Lib where

import Control.Applicative
import Data.Char

newtype Parser a = Parser {run :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

char x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

str = sequenceA . map char

spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

int :: Parser Int
int = read <$> notNull (spanP isDigit)

word :: Parser [Char]
word = notNull $ spanP isLetter

ws = spanP isSpace

sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []