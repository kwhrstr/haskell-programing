{-# LANGUAGE OverloadedStrings #-}
{- This tutorial is Chapter 24-}
module Text.Ch24_Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Data.Attoparsec.Text (parseOnly)
import Data.String (IsString)
import Text.Trifecta
import Data.Text (unpack)

badFraction :: IsString s => s
badFraction = "1/0"
alsoBad :: IsString s => s
alsoBad = "10"
shouldWork :: IsString s => s
shouldWork = "1/2"
shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return $ numerator % denominator
  
{-
main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  print $ parseFraction' badFraction
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad

-}

main :: IO ()
main = do
  let attoP = parseOnly parseFraction
  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad
  let p f = parseString f mempty
  print $ p parseFraction badFraction
  print $ p parseFraction shouldWork
  print $ p parseFraction shouldAlsoWork
  print $ p parseFraction alsoBad

  
