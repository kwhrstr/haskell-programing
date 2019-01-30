{-# LANGUAGE LambdaCase #-}
{- This tutorial is Chapter 24-}
module Ch24_LearnParsers where

import Text.Trifecta

type Token = Char
newtype Parser'' a = P([Token] -> [(a, [Token])])
type Parser'  = Parser''

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneTwo = char '1' >> char '2'
two = char '2'
three = char '3'
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"
  
pNL s = putStrLn $ '\n':s

p123 :: String -> IO ()
p123 s = print $ parseString (string s) mempty "123"

oneS :: Parser String
oneS = string "1"

oneTwoS :: Parser String
oneTwoS = string "12"

oneTwoThreeS :: Parser String
oneTwoThreeS = string "123"

testParse' :: (Show a) => Parser a -> IO ()
testParse' p = print $ parseString p mempty "123"



main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "one >> EOF:"
  testParse' (one >> eof)
  pNL "oneTwo >> EOF"
  testParse' (oneTwo >> eof)
  pNL "string \"1\", \"12\", \"123\""
  testParse' (choice [ oneTwoThreeS
                     , oneTwoS
                     , oneS
                     , stop ])
  pNL "char \"1\", \"12\", \"123\""
  testParse (choice [ one >> two >> three
                    , one >> two
                    , one
                    , stop ])
-- rudimentary char
-- demo only, this won't work as is.
char' :: Char -> Parser' Char
char' c =
  P $ \case
    (x:xs) -> [(c, xs) | c == x]
    _ -> []