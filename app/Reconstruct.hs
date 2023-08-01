module Reconstruct where

import qualified Data.Bimap as B
import qualified Maps
import qualified Symbols as S
import Tokeniser

returnToString :: [S.Symbol] -> String
returnToString =
  concatMap
    ( \token -> case token of
        S.Identifier str -> " " ++ str ++ " "
        S.LiteralString str -> "\"" ++ str ++ "\""
        S.LiteralNumber x -> "1.0"
        _ -> case B.lookupR token Maps.multiBimap of
          Just str -> padIf (shouldBePadded token) str " "
          Nothing -> case B.lookupR token Maps.singleBimap of
            Just ch -> [ch]
            Nothing -> error "This is illegal!"
    )

padIf :: Bool -> String -> String -> String
padIf cond str with =
  if cond
    then with ++ str ++ with
    else str

pad :: String -> String -> String
pad = padIf True

shouldBePadded :: S.Symbol -> Bool
shouldBePadded S.And = True
shouldBePadded S.Break = True
shouldBePadded S.Do = True
shouldBePadded S.Else = True
shouldBePadded S.Elseif = True
shouldBePadded S.End = True
shouldBePadded S.False = True
shouldBePadded S.For = True
shouldBePadded S.Function = True
shouldBePadded S.Goto = True
shouldBePadded S.If = True
shouldBePadded S.In = True
shouldBePadded S.Local = True
shouldBePadded S.Nil = True
shouldBePadded S.Not = True
shouldBePadded S.Or = True
shouldBePadded S.Repeat = True
shouldBePadded S.Return = True
shouldBePadded S.Then = True
shouldBePadded S.True = True
shouldBePadded S.Until = True
shouldBePadded S.While = True
shouldBePadded _ = False
