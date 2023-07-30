module Tokeniser where

import qualified Symbols as S
import Maps
import Data.Char (isSpace)
import Data.Maybe (isJust, fromJust)
import qualified Data.Bimap as M

import Debug.Trace

data Token
  = KeywordToken S.Keyword 
  | OperatorToken S.Operator
  | MetaToken S.Meta
  | LiteralToken S.Literal
  | Identifier String
  deriving (Show)

data TokeniserState = TokeniserState {
  _source :: String,
  _length :: Int,
  _pos :: Int,
  _accum :: String,
  _tokens :: [Token],
  _return :: Bool
}

initTokeniser :: String -> TokeniserState
initTokeniser source = TokeniserState {
  _source = source,
  _length = length source,
  _pos = 0,
  _accum = "",
  _tokens = [],
  _return = False
}

matchUserToken :: String -> Token
matchUserToken str = Identifier str

matchToken :: String -> Maybe Token
matchToken str
  | M.member str keywordBimap = Just $ KeywordToken $ keywordBimap M.! str
  | M.member str operatorBimap = Just $ OperatorToken $ operatorBimap M.! str
  | M.member str metaBimap = Just $ MetaToken $ metaBimap M.! str
  | otherwise = Nothing

tokenise :: TokeniserState -> [Token]
tokenise state
  | isEOF && not (null accum) = tokens ++ [matchUserToken accum]
  | isEOF = tokens
  | isJust langToken = tokenise state { _pos = nextPos, _accum = "", _tokens = tokens ++ [fromJust langToken]}
  | isSpace ch && not (null accum) = tokenise state { _pos = nextPos, _accum = "", _tokens = tokens ++ [matchUserToken accum]} 
  | isSpace ch = tokenise state { _pos = nextPos }
  | otherwise = tokenise state { _pos = nextPos, _accum = nextAccum}
  where isEOF = nextPos >= _length state
        accum = _accum state
        tokens = _tokens state
        ch = _source state !! _pos state
        nextPos = _pos state + 1
        nextAccum = accum ++ [ch]
        langToken = matchToken accum
