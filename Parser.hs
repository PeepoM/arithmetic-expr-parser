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

-- Primitive parsers

item :: Parser Char
item = Parser f
  where
    f [] = Nothing
    f (x : xs) = Just (x, xs)

-- Parse a character that satisfies a given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  do
    i <- item
    if p i
      then return i
      else empty

-- Parse a character of choice
char :: Char -> Parser Char
char c = satisfy (== c)

-- Parse an integer
int :: Parser Int
int = char '-' *> (negate <$> posInt) <|> posInt

-- Parse a positive integer
posInt :: Parser Int
posInt = Parser f
  where
    f xs
      | null num = Nothing
      | otherwise = Just (read num, rest)
      where
        (num, rest) = span isDigit xs

-- Parse zero or more spaces
spaces :: Parser String
spaces = many $ satisfy isSpace

--------------------

-- Grammar for the expression parser

-- expr ::= ter + expr | term - expr | term
-- term ::= factor * term | factor / term | factor
-- factor ::= (expr) | int
-- int ::= ... | -1 | 0 | 1 | ...

parseExpr :: Parser Int
parseExpr =
  liftA2 (+) parseTerm (char '+' *> parseExpr)
    <|> liftA2 (-) parseTerm (char '-' *> parseExpr)
    <|> parseTerm

parseTerm :: Parser Int
parseTerm =
  liftA2 (*) parseFactor (char '*' *> parseTerm)
    <|> liftA2 div parseFactor (char '/' *> parseTerm)
    <|> parseFactor

parseFactor :: Parser Int
parseFactor = char '(' *> parseExpr <* char ')' <|> int

eval :: String -> Int
eval s = case runParser parseExpr (filter (not . isSpace) s) of
  Just (x, []) -> x
  Just (x, xs) -> error ("Didn't parse the expression: " ++ xs)
  Nothing -> error "Parser failed!"