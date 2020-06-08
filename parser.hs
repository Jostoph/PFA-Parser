module Parser where

import Control.Applicative
import Data.Char
import Data.Map.Strict as Map

data Parser a = Parser (String -> [(a, String)])
data Expression = 
  Value Int |
  Operator Expression Char Expression |
  Group Char Expression Char |
  Variable String |
  Definition String Expression Char Expression String Expression deriving Show

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

split' _ [] = []
split' p (c:cs) = if p c then [(c, cs)] else []

char c = Parser (split' (==c))
digit = Parser (split' isDigit)
letter = Parser (split' isLetter)
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs
space = many $ char ' '
ident = (:) <$> letter <*> (many $ letter <|> digit)

term = space *> (def <|> Operator <$> facteur <* space <*> char '+' <*> term <|> facteur)
def = Definition <$> string "let" <* space <*> var <* space <*> char '=' <*> term <* space <*> string "in" <*> term
facteur = Operator <$> expression <* space <*> char '*' <*> facteur <|> expression
expression = space *> (entier <|> group <|> var)
entier = (Value . read) <$> some digit
group = Group <$> char '(' <*> term <* space <*> char ')'
var = Variable <$> ident

lookup' v env = case Map.lookup v env of
  Nothing -> error $ "Undefined variable " ++ v
  Just n -> n

addvar v e e1 env = eval' e1 (Map.insert v (eval' e env) env)

op '+' = (+)
op '*' = (*)

eval' :: Expression -> Map [Char] Int -> Int
eval' (Value v) = pure v
eval' (Operator e1 o e2) = op o <$> eval' e1 <*> eval' e2
eval' (Group _ e _) = eval' e
eval' (Variable v) = lookup' v
eval' (Definition _ (Variable v) _ e _ e1) = addvar v e e1

interpret s = eval' (fst $ head $ parse term s) Map.empty
