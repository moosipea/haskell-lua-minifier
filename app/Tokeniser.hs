module Tokeniser where

import qualified Data.Bimap as B (lookup, member)
import Data.Char (isDigit, isSpace)
import Data.List (groupBy)
import Data.Maybe (fromJust, isJust)
import Debug.Trace
import qualified Maps
import qualified Symbols as S

data TokenKind
  = Single S.Symbol
  | Multi String
  | LiteralString String
  | LiteralNumber String
  deriving (Show)

type Token = (Int, TokenKind)

tokeniseNumber :: String -> Int -> Token
tokeniseNumber input pos = (length content, LiteralNumber content)
  where
    content = takeWhile isDigit $ drop pos input

nonEscapedQuote :: (Char, Char) -> Bool
nonEscapedQuote (c0, c1) = not $ c0 /= '\\' && c1 == '\"'

tokeniseString :: String -> Int -> Token
tokeniseString input pos = (length content, LiteralString content)
  where
    fromPos = drop pos input
    pairs = zip fromPos (tail fromPos)
    content = map snd (takeWhile nonEscapedQuote pairs)

shouldEndMulti :: Char -> Bool
shouldEndMulti ch = not (isSpace ch) && not (B.member ch Maps.singleBimap)

tokeniseMulti :: String -> Int -> Token
tokeniseMulti input pos = (length content, Multi content)
  where content = takeWhile shouldEndMulti $ drop pos input

next :: String -> Int -> [Token] -> [Token]
next input pos tokens
  | pos >= length input = tokens
  | isSpace ch = next input (pos + 1) tokens
  | isDigit ch = next input (pos + fst numberToken) (tokens ++ [numberToken])
  | ch == '\"' = next input (pos + 2 + fst stringToken) (tokens ++ [stringToken])
  | isJust singleToken = next input (pos + 1) (tokens ++ [(1, Single $ fromJust singleToken)])
  | otherwise = next input (pos + fst multiToken) (tokens ++ [multiToken])
  where
    ch = input !! pos
    numberToken = tokeniseNumber input pos
    stringToken = tokeniseString input pos
    singleToken = B.lookup ch Maps.singleBimap :: Maybe S.Symbol
    multiToken = tokeniseMulti input pos

tok :: String -> [Token]
tok input = next input 0 []
