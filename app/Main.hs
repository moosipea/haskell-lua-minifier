module Main where

import Tokeniser
import qualified Maps
import qualified Data.Bimap as B

returnToString :: Token -> String
returnToString token = case snd token of
  Single symbol -> [Maps.singleBimap B.!> symbol]
  Multi value -> value
  LiteralString value -> ['\"'] ++ value ++ ['\"']
  LiteralNumber value -> value

tokensToString :: [Token] -> String
tokensToString tokens = unwords (map returnToString tokens)

main :: IO ()
main = do
  inputString <- readFile "test.lua"
  print . tokensToString $ tok inputString
