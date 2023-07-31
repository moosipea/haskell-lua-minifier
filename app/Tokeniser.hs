module Tokeniser where

import qualified Data.Bimap as B (lookup, member)
import Data.Char (isDigit, isSpace)
import Data.List (groupBy)
import Data.Maybe (fromJust, isJust)
import Debug.Trace
import qualified Maps
import qualified Symbols as S

data RawToken
  = Single S.Symbol
  | Multi String
  | LiteralString String
  | LiteralNumber String
  deriving (Show)

type PToken = (Int, RawToken)

tokeniseNumber :: String -> Int -> PToken
tokeniseNumber input pos = (length content, LiteralNumber content)
  where
    content = takeWhile isDigit $ drop pos input

nonEscapedQuote :: (Char, Char) -> Bool
nonEscapedQuote (c0, c1) = not $ c0 /= '\\' && c1 == '\"'

tokeniseString :: String -> Int -> PToken
tokeniseString input pos = (length content, LiteralString content)
  where
    fromPos = drop pos input
    pairs = zip fromPos (tail fromPos)
    content = map snd (takeWhile nonEscapedQuote pairs)

shouldEndMulti :: Char -> Bool
shouldEndMulti ch = not (isSpace ch) && not (B.member ch Maps.singleBimap)

tokeniseMulti :: String -> Int -> PToken
tokeniseMulti input pos = (length content, Multi content)
  where content = takeWhile shouldEndMulti $ drop pos input

next :: String -> Int -> [PToken] -> [PToken]
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

assignSymbol :: RawToken -> S.Symbol
assignSymbol token = case token of
  Single sym -> sym
  Multi raw -> case B.lookup raw Maps.multiBimap of
    Just sym -> sym
    Nothing -> S.Identifier raw
  LiteralString str -> S.LiteralString str
  LiteralNumber str -> S.LiteralNumber $ read str

tok :: String -> [S.Symbol]
tok input = map (assignSymbol . snd) $ next input 0 []
