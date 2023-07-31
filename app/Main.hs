module Main where

import Tokeniser

main :: IO ()
main = print . tok $ "local burger = 42"
