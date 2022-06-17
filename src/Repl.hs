module Repl (ParseError (..), Expr (..), ExprError (..), ReplError (..), Env, defaultEnv, parseAndEval) where

import Control.Monad (foldM, zipWithM)
import Data.Map qualified as Map
import Text.Read (readMaybe)

type Token = String

data ParseError
  = EmptyTokenList
  | InvalidRightParens
  | MissingRightParens
  | TokensRemaining
  deriving (Show, Eq)

data Expr
  = Boolean Bool
  | Number Float
  | Symbol String
  | Function String (Env -> [Expr] -> ExprResult)
  | List [Expr]
  | Lambda Expr Expr

instance Show Expr where
  show (Boolean x) = show x
  show (Number x) = show x
  show (Symbol x) = x
  show (Function x _) = "Function " ++ x
  show (List x) = "(" ++ unwords (map show x) ++ ")"
  show (Lambda x y) = show x ++ " " ++ show y

instance Eq Expr where
  (==) (Boolean x) (Boolean y) = x == y
  (==) (Number x) (Number y) = x == y
  (==) (Symbol x) (Symbol y) = x == y
  (==) (Function x _) (Function y _) = x == y
  (==) (List x) (List y) = x == y
  (==) (Lambda x1 x2) (Lambda y1 y2) = x1 == y1 && x2 == y2
  (==) _ _ = False

data ExprError
  = NotANumber
  | UnknownSymbol
  | UnexpectedFunction
  | InvalidForm
  deriving (Show, Eq)

-- A symbol -> expression lookup table. Calling def adds to this table
type Env = Map.Map String Expr

type ExprResult = Either ExprError (Expr, Env)

data ReplError
  = ParseError ParseError
  | ExprError ExprError
  deriving (Show, Eq)

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

-- A standard environment
defaultEnv :: Env
defaultEnv =
  Map.fromList
    [ ("+", Function "+" $ standardAritmeticOperator (+) $ Number 0),
      ("-", Function "-" $ subtractionOperator),
      ("*", Function "*" $ standardAritmeticOperator (*) $ Number 1),
      ("/", Function "/" $ divisionOperator),
      (">", Function ">" $ comparisonOperator (>)),
      ("<", Function "<" $ comparisonOperator (<)),
      (">=", Function ">=" $ comparisonOperator (>=)),
      ("<=", Function "<=" $ comparisonOperator (<=)),
      ("=", Function "=" $ comparisonOperator (==)),
      ("if", Function "if" $ ifOperator),
      ("def", Function "def" $ defOperator),
      ("lambda", Function "lambda" $ lambdaOperator)
    ]

-- Folds over [startValue : exprs] using operator
standardAritmeticOperator :: (Float -> Float -> Float) -> Expr -> Env -> [Expr] -> ExprResult
standardAritmeticOperator operator startValue env exprs = do
  evalStartValue <- eval env startValue
  evalExprs <- mapM (eval env) exprs
  foldM oper evalStartValue evalExprs
  where
    oper (Number x, env1) (Number y, env2) = Right $ (Number $ operator x y, Map.union env1 env2)
    oper _ _ = Left NotANumber

-- Subtraction has a few unique cases over the standard:
--  (- x) => 0 - x    x is subtracted from starting value
--  (- x y) => x - y  x is used AS starting value
subtractionOperator :: Env -> [Expr] -> ExprResult
subtractionOperator env [] = Right (Number 0, env)
subtractionOperator env (expr : []) = standardAritmeticOperator (-) (Number 0) env [expr]
subtractionOperator env (expr : exprs) = standardAritmeticOperator (-) expr env exprs

-- Division has a few unique cases over the standard:
--  (/ x) => 1 / x    x is divisor for starting value
--  (/ x y) => x / y  x is used AS starting value
divisionOperator :: Env -> [Expr] -> ExprResult
divisionOperator env [] = Right (Number 1, env)
divisionOperator env (expr : []) = standardAritmeticOperator (/) (Number 1) env [expr]
divisionOperator env (expr : exprs) = standardAritmeticOperator (/) expr env exprs

-- Checks that operator holds true between each adjacent pair
comparisonOperator :: (Float -> Float -> Bool) -> Env -> [Expr] -> ExprResult
comparisonOperator operator env exprs = do
  evalExprs <- mapM (eval env) exprs
  comparisons <- zipWithM oper evalExprs (drop 1 evalExprs)
  case all (== Boolean True) (map fst comparisons) of
    True -> Right $ (Boolean True, env)
    False -> Right $ (Boolean False, env)
  where
    oper :: (Expr, Env) -> (Expr, Env) -> ExprResult
    oper (Number x, env1) (Number y, env2) = Right (Boolean $ operator x y, Map.union env1 env2)
    oper _ _ = Left NotANumber

-- True => use first expr, False => use second expr
ifOperator :: Env -> [Expr] -> ExprResult
ifOperator env (x : y : z : []) = do
  (evalX, envX) <- eval env x
  (evalY, envY) <- eval env y
  (evalZ, envZ) <- eval env z
  case evalX of
    Boolean b -> case b of
      True -> Right (evalY, Map.union envX envY)
      False -> Right (evalZ, Map.union envX envZ)
    _ -> Left InvalidForm
ifOperator _ _ = Left InvalidForm

-- returns new env with new symbol added
defOperator :: Env -> [Expr] -> ExprResult
defOperator env (Symbol s : x : []) = do
  (evalX, newEnv) <- eval env x
  Right (evalX, Map.insert s evalX newEnv)
defOperator _ _ = Left InvalidForm

-- lambda is called by passing exprs after lambda
-- ((lambda (a b) (+ a b)) 2 3) => 5
lambdaOperator :: Env -> [Expr] -> ExprResult
lambdaOperator env (x : y : []) = Right (Lambda x y, env)
lambdaOperator _ _ = Left InvalidForm

-- Evaluates an expression
eval :: Env -> Expr -> ExprResult
eval env (Boolean x) = Right $ (Boolean x, env)
eval env (Number x) = Right $ (Number x, env)
eval env (Symbol x)
  | Map.member x env = Right $ (env Map.! x, env)
  | otherwise = Left UnknownSymbol
eval _ (Function _ _) = Left UnexpectedFunction
eval env (List []) = Right $ (List [], env)
eval env (List (x : xs)) = case eval env x of
  Right (Function _ f, newEnv) -> f newEnv xs
  Right (Lambda (List parms) f, newEnv) -> do
    case length parms == length xs of
      True -> do
        let nameValPairs = zipWith (\name val -> [name, val]) parms xs
        defs <- mapM (defOperator newEnv) nameValPairs
        let lambdaEnv = Map.unions (map snd defs)
        evalLambda <- eval lambdaEnv f
        return (fst evalLambda, newEnv)
      False -> Left $ InvalidForm
  Left exprError -> Left exprError
  _ -> Left InvalidForm
eval _ (Lambda _ _) = Left UnexpectedFunction

-- Simplifies user interface into one function call
parseAndEval :: Env -> String -> Either ReplError (Expr, Env)
parseAndEval env input =
  case parse . tokenize $ input of
    Right (expr, []) -> case eval env expr of
      Right result -> Right result
      Left e -> Left $ ExprError e
    Right (_, _) -> Left $ ParseError TokensRemaining
    Left e -> Left $ ParseError e