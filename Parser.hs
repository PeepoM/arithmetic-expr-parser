module Parser where

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

first :: (a -> c) -> (a, b) -> (c, b)
first f (a, b) = (f a, b)

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (first f) . p
