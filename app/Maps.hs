module Maps where

import Data.Bimap (Bimap)
import qualified Data.Bimap as B
import qualified Symbols as S

singleBimap :: Bimap Char S.Symbol
singleBimap
  = B.insert '+' S.Plus
  . B.insert '-' S.Minus
  . B.insert '*' S.Multiply
  . B.insert '/' S.Divide
  . B.insert '%' S.Modulo
  . B.insert '^' S.Power
  . B.insert '#' S.Length
  . B.insert '&' S.BitwiseAnd
  . B.insert '~' S.BitwiseXor
  . B.insert '|' S.BitwiseOr
  . B.insert '<' S.LessThan
  . B.insert '>' S.GreaterThan
  . B.insert '=' S.Equals
  . B.insert '(' S.OpenParentheses
  . B.insert ')' S.CloseParentheses
  . B.insert '{' S.OpenCurly
  . B.insert '}' S.CloseCurly
  . B.insert '[' S.OpenSquare
  . B.insert ']' S.CloseSquare
  . B.insert ';' S.Semicolon
  . B.insert ':' S.Colon
  . B.insert ',' S.Comma
  . B.insert '.' S.Dot
  $ B.empty 
