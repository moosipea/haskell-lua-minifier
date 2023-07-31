module Tokeniser where

import qualified Symbols as S

data Token
  = Single Char
  | Multi String
  | Literal String
  deriving (Show)

tok :: String -> [Token]
tok = undefined
