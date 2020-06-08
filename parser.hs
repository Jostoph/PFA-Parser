module Parser where

import Control.Applicative
import Data.Char
import Data.Map.Strict as Map

-- Parsing --

-- parser
data Parser a = Parser (String -> [(a, String)])

-- expressions
data Expression = 
  Value Int |
  Operator Expression Char Expression |
  Variable String |
  Definition String Expression Char Expression String Expression deriving Show

-- parsing function
parse (Parser p) cs = p cs

{- 
  Implementation of Functor, Applicative and Alternative for the Parser
-}
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

-- spliting function
split' _ [] = []
split' p (c:cs) = if p c then [(c, cs)] else []

{-
  Simple parsing functions to assemble more complex grammar parsers
-}
char c = Parser (split' (==c))
digit = Parser (split' isDigit)
letter = Parser (split' isLetter)
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs
space = many $ char ' '
-- ident (identifier) : at least 1 letter followed by 0 or more letters or digits
ident = (:) <$> letter <*> (many $ letter <|> digit)

{-
  Grammar Parsers
  Parsing functions defining the language's syntax
-}
term = space *> (def <|> Operator <$> facteur <* space <*> char '+' <*> term <|> facteur)
def = Definition <$> string "let" <* space <*> var <* space <*> char '=' <*> term <* space <*> string "in" <*> term
facteur = Operator <$> expression <* space <*> char '*' <*> facteur <|> expression
expression = space *> (entier <|> group <|> var)
entier = (Value . read) <$> some digit
group = char '(' *> term <* space <* char ')'
var = Variable <$> ident

-- Evaluation --

-- function to lookup a given variable in an environment
lookup' v env = case Map.lookup v env of
  Nothing -> error $ "Undefined variable " ++ v
  Just n -> n

-- function to insert a new variable in the environment after evaluating it in the current env
addvar v e e1 env = eval' e1 (Map.insert v (eval' e env) env)

-- operator selection function
op '+' = (+)
op '*' = (*)

-- evaluation function with implicit environment
eval' :: Expression -> Map [Char] Int -> Int
eval' (Value v) = pure v
eval' (Operator e1 o e2) = op o <$> eval' e1 <*> eval' e2
eval' (Variable v) = lookup' v
eval' (Definition _ (Variable v) _ e _ e1) = addvar v e e1

{-
  function to interpret a string using a term parser and return the result.
  The start environment is empty.
  If the string could not get parsed to the end, rise error.
-}
interpret s = 
  let
    expList = parse term s
  in
    if Prelude.null expList || not (Prelude.null $ snd $ head expList) then
      error $ "syntax error"
    else
      eval' (fst $ head expList) Map.empty
