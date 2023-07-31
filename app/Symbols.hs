module Symbols where

data Symbol
  = And -- keywords start
  | Break
  | Do
  | Else
  | Elseif
  | End
  | False
  | For
  | Function
  | Goto
  | If
  | In
  | Local
  | Nil
  | Not
  | Or
  | Repeat
  | Return
  | Then
  | True
  | Until
  | While -- keywords end
  | Plus
  | Minus
  | Multiply
  | Divide
  | Modulo
  | Power
  | Length
  | BitwiseAnd
  | BitwiseXor
  | BitwiseOr
  | ShiftLeft
  | ShiftRight
  | FloorDivide
  | Equals
  | XorAssign
  | LessThanEqual
  | GreaterThanEqual
  | LessThan
  | GreaterThan
  | Assign
  | OpenParentheses
  | CloseParentheses
  | OpenCurly
  | CloseCurly
  | OpenSquare
  | CloseSquare
  | Label
  | Semicolon
  | Colon
  | Comma
  | Dot
  | Concat
  | VarArgs
  | Identifier String
  deriving (Eq, Ord, Show)
