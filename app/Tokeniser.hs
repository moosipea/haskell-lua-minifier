module Tokeniser where

import Symbols
import Maps
import Data.Char (isSpace)
import qualified Data.Bimap as M

data Token
  = KeywordToken Keyword 
  | OperatorToken Operator
  | MetaToken Meta
  | LiteralToken Literal
  | Identifier String
  deriving (Show)

data TokeniserState = TokeniserState {
  _source :: String,
  _length :: Int,
  _pos :: Int,
  _accum :: String,
  _tokens :: [Token]
}

initTokeniser :: String -> TokeniserState
initTokeniser source = TokeniserState {
  _source = source,
  _length = length source,
  _pos = 0,
  _accum = "",
  _tokens = []
}

matchUserToken :: String -> Token
matchUserToken str = Identifier str

tokenise :: TokeniserState -> [Token]
tokenise state
  | isEOF && isAccumEmpty = tokens -- return
  | isEOF = tokenise state { _accum = "" }
  | M.member accum keywordBimap = tokenise state { _pos = nextPos, _accum = "", _tokens = tokens ++ [KeywordToken $ keywordBimap M.! accum] }
  | M.member accum operatorBimap = tokenise state { _pos = nextPos, _accum = "", _tokens = tokens ++ [OperatorToken $ operatorBimap M.! accum] }
  | M.member accum metaBimap = tokenise state { _pos = nextPos, _accum = "", _tokens = tokens ++ [MetaToken $ metaBimap M.! accum] }
  | isSpace ch && not isAccumEmpty = tokenise state { _pos = nextPos, _accum = "", _tokens = tokens ++ [matchUserToken accum]} 
  | isSpace ch = tokenise state { _pos = nextPos }
  | otherwise = tokenise state { _pos = nextPos, _accum = nextAccum}
  where isEOF = nextPos >= _length state
        isAccumEmpty = null accum
        accum = _accum state
        tokens = _tokens state
        ch = _source state !! _pos state
        nextPos = _pos state + 1
        nextAccum = accum ++ [ch]
