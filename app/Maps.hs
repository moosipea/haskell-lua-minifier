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

multiBimap :: Bimap String S.Symbol
multiBimap 
  = B.insert "and" S.And
  . B.insert "break" S.Break
  . B.insert "do" S.Do
  . B.insert "else" S.Else
  . B.insert "elseif" S.Elseif
  . B.insert "end" S.End
  . B.insert "false" S.False
  . B.insert "for" S.For
  . B.insert "function" S.Function
  . B.insert "goto" S.Goto
  . B.insert "if" S.If
  . B.insert "in" S.In
  . B.insert "local" S.Local
  . B.insert "nil" S.Nil
  . B.insert "not" S.Not
  . B.insert "or" S.Or
  . B.insert "repeat" S.Repeat
  . B.insert "return" S.Return
  . B.insert "then" S.Then
  . B.insert "true" S.True
  . B.insert "until" S.Until
  . B.insert "while" S.While
  . B.insert "<<" S.ShiftLeft
  . B.insert ">>" S.ShiftRight
  . B.insert "//" S.FloorDivide
  . B.insert "==" S.Equals
  . B.insert "~=" S.XorAssign
  . B.insert "<=" S.LessThanEqual
  . B.insert ">=" S.GreaterThanEqual
  . B.insert "::" S.Label
  . B.insert ".." S.Concat
  . B.insert "..." S.VarArgs
  $ B.empty
