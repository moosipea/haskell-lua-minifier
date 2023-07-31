module Main where

import Tokeniser
import qualified Maps
import qualified Data.Bimap as B

returnToString :: PToken -> String
returnToString token = case snd token of
  Single symbol -> [Maps.singleBimap B.!> symbol]
  Multi value -> value
  LiteralString value -> ['\"'] ++ value ++ ['\"']
  LiteralNumber value -> value

tokensToString :: [PToken] -> String
tokensToString tokens = unwords (map returnToString tokens)

main :: IO ()
main = do
  inputString <- readFile "test.lua"
  print $ tok inputString
