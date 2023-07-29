module Maps where

import Data.Bimap (Bimap)
import qualified Data.Bimap as M
import qualified Symbols as S

keywordBimap :: Bimap String S.Keyword
keywordBimap
  = M.insert "and" S.And
  . M.insert "break" S.Break
  . M.insert "do" S.Do
  . M.insert "else" S.Else
  . M.insert "elseif" S.Elseif
  . M.insert "end" S.End
  . M.insert "false" S.False
  . M.insert "for" S.For
  . M.insert "function" S.Function
  . M.insert "goto" S.Goto
  . M.insert "if" S.If
  . M.insert "in" S.In
  . M.insert "local" S.Local
  . M.insert "nil" S.Nil
  . M.insert "not" S.Not
  . M.insert "or" S.Or
  . M.insert "repeat" S.Repeat
  . M.insert "return" S.Return
  . M.insert "then" S.Then
  . M.insert "true" S.True
  . M.insert "until" S.Until
  . M.insert "while" S.While
  $ M.empty 

operatorBimap :: Bimap String S.Operator
operatorBimap 
  = M.insert "+" S.Add
  . M.insert "-" S.SubtractOrMinus
  . M.insert "*" S.Multiply
  . M.insert "/" S.Divide
  . M.insert "%" S.Modulo
  . M.insert "^" S.Power
  . M.insert "#" S.Length
  . M.insert "&" S.BitwiseAnd
  . M.insert "~" S.BitwiseXor
  . M.insert "|" S.BitwiseOr
  . M.insert "<<" S.LeftShift
  . M.insert ">>" S.RightShift
  . M.insert "//" S.FloorDivide
  . M.insert "==" S.Equals
  . M.insert "~=" S.XorAssign
  . M.insert "<=" S.LessThanEqual
  . M.insert ">=" S.GreaterThanEqual
  . M.insert "<" S.LessThan
  . M.insert ">" S.GreaterThan
  . M.insert "=" S.Assign
  . M.insert "." S.Dot
  . M.insert ".." S.Concatinate
  . M.insert ":" S.Colon
  $ M.empty
