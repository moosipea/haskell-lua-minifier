module Tokeniser where

import Data.Char (isDigit)
import Data.List (groupBy)
import qualified Symbols as S

data TokenKind
  = Single Char
  | Multi String
  | LiteralString String
  | LiteralNumber String
  deriving (Show)

type Token = (Int, TokenKind)

tokeniseNumber :: String -> Int -> Maybe Token
tokeniseNumber input pos =
  if null content
    then Nothing
    else Just (length content, LiteralNumber content)
  where
    content = takeWhile isDigit $ drop pos input

nonEscapedQuote :: (Char, Char) -> Bool
nonEscapedQuote (c0, c1) = not $ (c0 /= '\\') && (c1 == '\"')

tokeniseString :: String -> Int -> Maybe Token
tokeniseString input pos =
  if null content
    then Nothing
    else Just (length content, LiteralString content)
  where
    fromPos = drop pos input
    pairs = zip fromPos (tail fromPos)
    content = fst (head pairs) : map snd (takeWhile nonEscapedQuote pairs)

tok :: String -> [Token]
tok = undefined
