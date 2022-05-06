module Main where

import Control.Monad (forever)

main :: IO ()
main = forever $ do
    input <- getLine
    putStrLn input
    -- print.eval.parse.tokenize input