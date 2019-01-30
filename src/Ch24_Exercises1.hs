{-# LANGUAGE OverloadedStrings #-}
module Ch24_Exercises1 where

import Text.Trifecta
import Control.Applicative


data NumberOrString =
   NOSS String
 | NOSI Integer
 deriving (Show, Eq)
 
type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)
instance Ord SemVer where
  compare (SemVer mj mn p _ _)  (SemVer mj' mn'  p' _ _) =
    compare mj mj' <>
    compare mn mn' <>
    compare p p'
  


semVer :: String
semVer = "1.0.0-x.7.z.92"

{-
parseSemVer :: Parser SemVer
parseSemVer = do
  major <- decimal
  skipMany $ char '.'
  minor <- decimal
  skipMany $ char '.'
  patch <- decimal
  skipMany $ char '-'
  semRelease <- parseNOSES
  skipMany $ char '+'
  SemVer major minor patch semRelease <$> parseNOSES

-}

parsePrerelease :: Parser NumberOrString
parsePrerelease = skipMany (oneOf ".") >> parseNOS

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer <$> decimal <*>
            (char '.' *> decimal) <*>
            (char '.' *> decimal) <*>
            (char '-' *> some parsePrerelease <|> mempty) <*>
            (char '+' *> some parsePrerelease <|> mempty)

parseNOSES :: Parser [NumberOrString]
parseNOSES = many (parseNOS <* skipMany (char '.'))

parseNOS :: Parser NumberOrString
parseNOS = ( NOSI <$> try (decimal <* notFollowedBy letter)) <|>
           ( NOSS <$> some (letter <|> digit))

