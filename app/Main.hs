module Main where

import Tokeniser

main :: IO ()
main
  = putStrLn
  . show
  . tokenise
  $ "local burger = 42"
