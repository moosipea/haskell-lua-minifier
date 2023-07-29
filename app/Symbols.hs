module Symbols where

data Keyword 
  = And
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
  | While
  deriving (Eq, Ord, Show)

data Operator
  = Add
  | SubtractOrMinus -- Doesn't matter
  | Multiply
  | Divide 
  | Modulo
  | Power
  | Length
  | BitwiseAnd
  | BitwiseXor
  | BitwiseOr
  | LeftShift
  | RightShift
  | FloorDivide
  | Equals
  | XorAssign
  | LessThanEqual
  | GreaterThanEqual
  | LessThan
  | GreaterThan
  | Assign
  | Dot
  | Concatinate
  | Colon
  deriving (Eq, Ord)

data Meta
  = OpenParentheses
  | CloseParentheses
  | OpenCurly
  | CloseCurly
  | OpenSquare
  | CloseSquare
  | Label
  | Semicolon
  | Comma
  | VarArg
  deriving (Eq, Ord)
