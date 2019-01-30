{-# LANGUAGE QuasiQuotes #-}
module Ch24_AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ


type NumberOrString = Either Integer String
newtype MyName = MyName String deriving Show

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n") *>
  (Left <$> integer) <|>
  (Right <$> some letter) <*
  skipMany (oneOf "\n")
  

{-
main = do
  let p f = parseString f mempty
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c
-}

main = do
  let p f = parseString f mempty eitherOr
  print $ p (some $ token parseNos)

eitherOr :: String
eitherOr =[r|
123
abc
456
def
|]

--r :: QuasiQuoter
--r = undefined