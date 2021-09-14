module Parser where

import Control.Applicative

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  p1 <*> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> Nothing
    Just (f, fs) -> runParser (f <$> p2) fs

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 = Parser $ (<|>) <$> p1 <*> p2
