module Tokeniser where

import qualified Symbols as S
import Maps
import Data.Char (isSpace, isDigit)
import Data.Maybe (isJust, fromJust)
import qualified Data.Bimap as M
import Text.Read (readMaybe)

import Debug.Trace

data Token
  = KeywordToken S.Keyword 
  | OperatorToken S.Operator
  | MetaToken S.Meta
  | LiteralToken S.Literal
  | Identifier String
  deriving (Show)

matchUserToken :: String -> Token
matchUserToken str = Identifier str

matchLangToken :: String -> Maybe Token
matchLangToken str
  | M.member str keywordBimap = Just $ KeywordToken $ keywordBimap M.! str
  | M.member str operatorBimap = Just $ OperatorToken $ operatorBimap M.! str
  | M.member str metaBimap = Just $ MetaToken $ metaBimap M.! str
  | otherwise = Nothing

maybeAppend :: Maybe a -> [a] -> [a]
maybeAppend maybeElement elements = case maybeElement of
  Just element -> elements ++ [element]
  Nothing -> elements

includesLast :: Eq a => [a] -> [a] -> Bool
includesLast cmps xs
  | length cmps > length xs = False
  | otherwise = all (\(x, y) -> x == y) $ zip rcmps rxs 
  where rxs = reverse xs
        rcmps = reverse cmps

nextChar :: String -> String -> Int -> [Token] -> Int -> Bool -> Bool -> [Token]
nextChar src accum len tokens pos stringLiteral comment
  | eof && emptyAccum     = tokens                            -- return
  | eof                   = maybeAppend maybeLangToken tokens -- return
  | comment && newLine    = nextChar src accum len tokens (pos + 1) False False
  | spaceOutsideLiteral   = skip
  | comment               = skip
  | startComment          = nextChar src accum len tokens (pos + 1) False True
  | isJust maybeLangToken = traceShow "!!!adding langtoken!!!" $ nextChar src "" len (tokens ++ [fromJust maybeLangToken]) (pos + 1) False False
  | isJust maybeNumber && (not $ isDigit ch)  = nextChar src "" len (tokens ++ [parsedNumber]) (pos + 1) False False
  | otherwise             = nextChar src (accum ++ [ch]) len tokens (pos + 1) stringLiteral comment
  where eof = (pos + 1) >= len
        ch  = src !! pos
        newLine = ch == '\n'
        skip = nextChar src "" len tokens (pos + 1) stringLiteral comment
        startComment = includesLast "--" accum && not stringLiteral
        spaceOutsideLiteral = isSpace ch && not stringLiteral
        emptyAccum = null accum
        maybeLangToken = matchLangToken accum
        maybeNumber = readMaybe accum :: Maybe Float
        parsedNumber = LiteralToken $ S.NumberLiteral $ fromJust maybeNumber

tokenise :: String -> [Token]
tokenise src = nextChar src "" (length src) [] 0 False False
