module Main where

import Control.Monad (forever)
import Repl

main :: IO ()
main = forever $ do
  -- env needs to exists outside the loop. Use something else than forever monad?
  input <- getLine
  let env = defaultEnv
  case parseAndEval env input of
    (Right output, _) -> print output
    (Left output, _) -> print output