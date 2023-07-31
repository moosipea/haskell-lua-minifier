module Main where

import Tokeniser

main :: IO ()
--main = print $ tokeniseNumber "123aad" 0
main = print $ tokeniseString "hello, world!\"" 0
