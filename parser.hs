module Parser where

import Control.Applicative
import Data.Char

data Parser a = Parser (String -> [(a, String)])
data Expression = Value Int | Operator Expression Char Expression | Group Char Expression Char | Variable String deriving Show

parse (Parser p) cs = p cs

instance Functor Parser where
  fmap f (Parser p) = Parser (\cs -> [(f x, ys) | (x, ys) <- p cs])

instance Applicative Parser where
  pure x = Parser (\cs -> [(x, cs)])
  p <*> q = Parser (\cs -> [y | (x, ys) <- parse p cs, y <- parse (fmap x q) ys])

instance Alternative Parser where
  empty = Parser (\_ -> [])
  p <|> q = Parser (\cs -> case parse p cs of
    [] -> parse q cs
    ps -> ps)

split _ [] = []
split p (c:cs) = if p c then [(c, cs)] else []

char c = Parser (split (==c))
digit = Parser (split isDigit)
letter = Parser (split isLetter)
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs

code = term
-- definition = Definition <$> string "let" <*> var <*> char '=' <*> term <*> string "in" <*> code
term = Operator <$> facteur <*> char '+' <*> term <|> facteur
facteur = Operator <$> expression <*> char '*' <*> facteur <|> expression
expression = entier <|> group <|> var
entier = (Value . read) <$> some digit
group = Group <$> char '(' <*> term <*> char ')'
var = Variable <$> some letter
