module LearnReader where

import Data.Char
import GHC.Base


cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

fmapped :: String -> String
fmapped  = fmap cap rev

tupled :: String -> (String, String)
tupled = liftA2 (,) cap rev

tupled' :: String -> (String, String)
tupled' = (,) <$> rev <*> cap

tupled'' :: String -> (String, String)
tupled'' = do
  a <- cap
  b <- rev
  return (a, b)