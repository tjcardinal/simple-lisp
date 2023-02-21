module Main where

import Repl

main :: IO ()
main = do
  let env = defaultEnv
  replLoop env

replLoop :: Repl.Env -> IO ()
replLoop env = do
  input <- getLine
  case parseAndEval env input of
    Right (output, newEnv) -> do
      print output
      replLoop newEnv
    Left output -> do
      print output
      replLoop env