module ExercisesCh24_2 where

import Control.Applicative
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = char '0' <|>
             char '1' <|>
             char '2' <|>
             char '3' <|>
             char '4' <|>
             char '5' <|>
             char '6' <|>
             char '7' <|>
             char '8' <|>
             char '9'
parseDigit' :: Parser Char
parseDigit' = oneOf "0123456789" <?> "a digit between 0 and 9"

base10Integer :: Parser Integer
base10Integer =  read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
  option <- optional (char '-')
  xs <- some parseDigit
  case option of
    Nothing -> return $ read xs
    Just x -> return $ read (x:xs)