module Parser where

import Control.Applicative
import Data.Char (isDigit, isSpace)

-- Data type representation of a parser
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

-- Functor, Applicative and Monad instances enable parser chaining

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  p1 <*> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> Nothing
    Just (f, fs) -> runParser (f <$> p2) fs

instance Monad Parser where
  p >>= f = Parser $ \s -> case runParser p s of
    Nothing -> Nothing
    Just (x, xs) -> runParser (f x) xs

-- Alternative instance represents choice between parsers
instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ (<|>) <$> p1 <*> p2