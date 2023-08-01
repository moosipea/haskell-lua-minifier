module Main where

import Tokeniser
import Reconstruct (returnToString)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  inputString <- readFile $ head args
  print . returnToString $ tok inputString
