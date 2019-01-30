module Ch24_Exercises4 where

import           Text.Trifecta
import           Control.Applicative
type NumberingPlanArea = Integer
type Exchange = Integer
type LineNumber = Integer

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving(Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  optional (string "1-")
  optional (char '(')
  npa <- count 3 digit
  optional (char ')')
  optional (oneOf " -")
  exec <- count 3 digit
  optional (oneOf " -")
  ln <- count 4 digit
  eof
  return $ PhoneNumber (read npa) (read exec) (read ln)

