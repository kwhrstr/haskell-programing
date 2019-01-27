{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{- This tutorial is Chapter 24-}
module Data.Ini where

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import           Data.Char
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Text.IO as TIO
import           Text.RawString.QQ
import           Text.Trifecta
import           Test.Hspec


headerEx :: ByteString
headerEx = "[blah]"

newtype Header = Header String deriving (Eq, Ord, Show)

type Name = String
type Value = String
type Assignments = Map Name Value

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  skipComments
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEQL
  skipComments
  return (name, val)

parseAssignment' :: Parser (Name, Value)
parseAssignment' = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  return (name, val)

-- | SKipp end of line and
-- whitespace beyond
skipEQL :: Parser ()
skipEQL = skipMany (oneOf "\n")


commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
  "; blaf\n; woot\n \n;hah"

skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEQL)

sectionEx :: ByteString
sectionEx =
  "; ignore me \n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]


sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

; comment4
[whatisit]
red=intoothandclaw
|]

data Section = Section Header Assignments
  deriving (Eq, Show)

newtype Config = Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEQL
  assignments <- some parseAssignment
  skipEQL
  return $
    Section h (M.fromList assignments)

rollup :: Section
       -> Map Header Assignments
       -> Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return $ Config mapOfSections
  
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing


main :: IO ()
main = hspec $ do
  describe "Assignment Parsing" $
    it "can parse a simple assignment" $ do
      let m = parseByteString parseAssignment mempty assignmentEx
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just ("woot", "1")
  describe "Commnent parsing" $
    it "Skips comment before header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
          m = parseByteString p mempty i
          r' = maybeSuccess m
      print m
      r' `shouldBe` Just (Header "blah")
  describe "Section parsing" $
    it "can parse a simple section" $ do
      let m = parseByteString parseSection mempty sectionEx
          r' = maybeSuccess m
          states = M.fromList[("Chris", "Texas")]
          expected' = Just (Section (Header "states") states)
      print m
      r' `shouldBe` expected'
  describe "INI parsing" $
    it "Can parse multiple sections" $ do
      let m = parseByteString parseIni mempty sectionEx''
          r' = maybeSuccess m
          sectionValues = M.fromList [ ("alias", "claw")
                                     , ("host", "wikipedia.org")
                                     ]
          whatisitValues = M.fromList [("red", "intoothandclaw")]
          expected' = Just (Config
                              (M.fromList [ (Header "section", sectionValues)
                                          , (Header "whatisit", whatisitValues)
                                          ]
                              )
                           )
      print m
      r' `shouldBe` expected'


p' :: Parser [Integer]
p' = some $ do
  i <- token (some digit)
  return $ read i



