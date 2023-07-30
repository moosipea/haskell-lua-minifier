module Main where

import Tokeniser

main :: IO ()
main
  = putStrLn
  . show
  . tokenise
  $ initTokeniser "local burger = 42"
