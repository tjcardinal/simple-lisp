module MyLib (tokenize, parse) where

import Text.Read (readMaybe)

data Expr
  = Num Integer
  | Symbol String
  | List [Expr]

instance Show Expr where
  show (Num x) = show x
  show (Symbol x) = x
  show (List x) = "(" ++ unwords (map show x) ++ ")"

tokenize :: String -> [String]
tokenize = words . spread
  where
    spread "" = ""
    spread ('(' : xs) = ' ' : '(' : ' ' : spread xs
    spread (')' : xs) = ' ' : ')' : ' ' : spread xs
    spread (x : xs) = x : spread xs

parse :: [String] -> (Expr, [String])
parse (")" : xs) = error "Invalid )"
parse ("(" : xs) = (List exprs, remainder)
  where
    (exprs, remainder) = parseList [] xs
parse (x : xs) = (parseAtom x, xs)

parseList :: [Expr] -> [String] -> ([Expr], [String])
parseList exprs (")" : xs) = (exprs, xs)
parseList exprs ("(" : xs) = (exprs ++ [nextExprs], remainder)
  where
    (nextExprs, remainder) = parse xs
parseList exprs xs = (exprs ++ [nextExpr] ++ restExprs, listRemainder)
  where
    (nextExpr, remainder) = parse xs
    (restExprs, listRemainder) = parseList [] remainder

parseAtom :: String -> Expr
parseAtom x = case readMaybe x of
  Just x' -> Num x'
  Nothing -> Symbol x

eval :: Expr -> Expr
eval x = x