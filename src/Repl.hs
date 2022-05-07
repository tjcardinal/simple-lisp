module Repl (tokenize, parse) where

import Text.Read (readMaybe)

data Expr
  = List [Expr]
  | Num Integer
  | Symbol String

instance Show Expr where
  show (Num n) = show n
  show (Symbol s) = s
  show (List l) = "(" ++ unwords (map show l) ++ ")"

-- Tokenizes a string. Splits it by whitespace (discarded) and parens (kept)
tokenize :: String -> [String]
tokenize = words . spread
  where
    spread "" = ""
    spread ('(' : xs) = ' ' : '(' : ' ' : spread xs
    spread (')' : xs) = ' ' : ')' : ' ' : spread xs
    spread (x : xs) = x : spread xs

-- Parses a list of tokens to an expression.
-- Returns the expression and any remaining tokens
parse :: [String] -> (Expr, [String])
parse [] = error "Empty token list"
parse (")" : xs) = error "Invalid )"
parse ("(" : xs) = (List exprs, remainder)
  where
    (exprs, remainder) = parseList [] xs
parse (x : xs) = (parseAtom x, xs)

-- Parses a list of tokens to a list of expression.
-- Stops once a ) is found.
-- Appends the expressions to the input expression list
-- and returns the full list and any remaining tokens
parseList :: [Expr] -> [String] -> ([Expr], [String])
parseList exprs (")" : xs) = (exprs, xs)
parseList exprs xs = (exprs ++ [nextExpr] ++ restExprs, listRemainder)
  where
    (nextExpr, remainder) = parse xs
    (restExprs, listRemainder) = parseList [] remainder

-- Parses an atom token to an expression
parseAtom :: String -> Expr
parseAtom x = case readMaybe x of
  Just x' -> Num x'
  Nothing -> Symbol x

eval :: Expr -> Expr
eval x = x