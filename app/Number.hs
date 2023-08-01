module Number where

import Data.Char (toLower)
import Text.Read (readMaybe)

startsWith :: String -> String -> Bool
startsWith with str = (length str >= length with) && all (uncurry (==)) (zip with str)

parseLuaHex :: String -> Maybe Float
parseLuaHex str = if any (`elem` "p.") str
  then error $ "can't yet parse " ++ str
  else readMaybe str

parseLuaNumber :: String -> Maybe Float
parseLuaNumber "" = Nothing
parseLuaNumber str = if startsWith "0x" $ map toLower str
  then parseLuaHex str
  else readMaybe str
