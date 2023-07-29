module Tokeniser where

import Symbols
import Maps

data Token
  = KeywordToken Keyword 
  | OperatorToken Operator
  | MetaToken Meta
  | LiteralToken Literal
  | Identifier String

data TokeniserState = TokeniserState {
  pos :: Integer,
  accum :: String
}

initTokeniser :: TokeniserState
initTokeniser = TokeniserState {
  pos = 0,
  accum = ""
}

tokenise :: TokeniserState -> String -> [Token]
tokenise state source = undefined
