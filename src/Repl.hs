module Repl (defaultEnv, parseAndEval) where

import Control.Monad (foldM)
import Data.Map qualified as Map
import Text.Read (readMaybe)

type Token = String

data ParseError
  = EmptyTokenList
  | InvalidRightParens
  | MissingRightParens
  | TokensRemaining
  deriving (Show)

data Expr
  = Boolean Bool
  | Number Float
  | Symbol String
  | Function String (Env -> [Expr] -> Either ExprError Expr)
  | List [Expr]

instance Show Expr where
  show (Boolean x) = show x
  show (Number x) = show x
  show (Symbol x) = x
  show (Function x _) = "Function " ++ x
  show (List x) = "(" ++ unwords (map show x) ++ ")"

data ExprError
  = NotANumber
  | UnknownSymbol
  | UnexpectedFunction
  | InvalidForm
  deriving (Show)

type Env = Map.Map String Expr

data ReplError
  = ParseError ParseError
  | ExprError ExprError
  deriving (Show)

-- Tokenizes a string. Splits it by whitespace (discarded) and parens (kept)
tokenize :: String -> [Token]
tokenize = words . spread
  where
    spread "" = ""
    spread ('(' : xs) = ' ' : '(' : ' ' : spread xs
    spread (')' : xs) = ' ' : ')' : ' ' : spread xs
    spread (x : xs) = x : spread xs

-- Parses a list of tokens to an expression.
-- Returns the expression and any remaining tokens
parse :: [Token] -> Either ParseError (Expr, [Token])
parse [] = Left EmptyTokenList
parse (")" : _) = Left InvalidRightParens
parse ("(" : xs) = do
  (exprs, remainder) <- parseList xs
  return (List exprs, remainder)
parse (x : xs) = Right (parseAtom x, xs)

-- Parses a list of tokens to a list of expression.
-- Stops once a ) is found.
-- Appends the expressions to the input expression list
-- and returns the full list and any remaining tokens
parseList :: [Token] -> Either ParseError ([Expr], [Token])
parseList [] = Left MissingRightParens
parseList (")" : remainder) = Right ([], remainder)
parseList tokens = do
  (nextExpr, remainder) <- parse tokens
  (restExprs, listRemainder) <- parseList remainder
  return (nextExpr : restExprs, listRemainder)

-- Parses an atom token to an expression
parseAtom :: Token -> Expr
parseAtom "true" = Boolean True
parseAtom "false" = Boolean False
parseAtom x = case readMaybe x of
  Just n -> Number n
  Nothing -> Symbol x

defaultEnv :: Env
defaultEnv =
  Map.fromList
    [ ("+", Function "+" $ arithmeticOperator (+) $ Number 0),
      ("-", Function "-" $ arithmeticOperator (-) $ Number 0),
      ("*", Function "*" $ arithmeticOperator (*) $ Number 1),
      ("/", Function "/" $ arithmeticOperator (/) $ Number 1),
      (">", Function ">" $ comparisonOperator (>)),
      ("<", Function "<" $ comparisonOperator (<)),
      (">=", Function ">=" $ comparisonOperator (>=)),
      ("<=", Function "<=" $ comparisonOperator (<=)),
      ("=", Function "=" $ comparisonOperator (==)),
      ("if", Function "if" $ ifOperator)
    ]

arithmeticOperator :: (Float -> Float -> Float) -> Expr -> Env -> [Expr] -> Either ExprError Expr
arithmeticOperator operation baseCase env exprs = do
  evalExprs <- mapM (eval env) exprs
  foldM oper baseCase evalExprs
  where
    oper (Number x) (Number y) = Right $ Number $ operation x y
    oper _ _ = Left NotANumber

comparisonOperator :: (Float -> Float -> Bool) -> Env -> [Expr] -> Either ExprError Expr
comparisonOperator op env exprs = do
  evalExprs <- mapM (eval env) exprs
  let comparisons = zipWith oper evalExprs (drop 1 evalExprs)
  case all (== True) comparisons of
    True -> Right $ Boolean True
    False -> Right $ Boolean False
  where
    oper (Number y) (Number z) = op y z
    oper _ _ = False

ifOperator :: Env -> [Expr] -> Either ExprError Expr
ifOperator env (x:y:z:[]) = do
  evalX <- eval env x
  case evalX of
    Boolean b -> case b of
      True -> Right y
      False -> Right z
    _ -> Left InvalidForm
ifOperator _ _ = Left InvalidForm

-- Evaluates an expression
eval :: Env -> Expr -> Either ExprError Expr
eval _ (Boolean x) = Right $ Boolean x
eval _ (Number x) = Right $ Number x
eval env (Symbol x)
  | Map.member x env = Right $ env Map.! x
  | otherwise = Left UnknownSymbol
eval _ (Function _ _) = Left UnexpectedFunction
eval _ (List []) = Right $ List []
eval env (List (x : xs)) = case eval env x of
  Right (Function _ f) -> f env xs
  Left exprError -> Left exprError
  _ -> Left InvalidForm

parseAndEval :: Env -> String -> Either ReplError Expr
parseAndEval env input =
  case parse . tokenize $ input of
    Right (expr, []) -> case eval env expr of
      Right result -> Right result
      Left e -> Left $ ExprError e
    Right (_, _) -> Left $ ParseError TokensRemaining
    Left e -> Left $ ParseError e