{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Ch24_Exercises5 where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.List (genericLength)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf (printf)
import Text.RawString.QQ
import Text.Trifecta

logEx :: ByteString
logEx = [r|
-- wheee a comment
>> 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

>> 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

miniLog :: ByteString
miniLog = [r|
>> 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
|]

type Day = Integer
type Month = Integer 
type Year = Integer 
type Activity = String 

newtype Time = Time Integer deriving (Eq, Ord, Num)
data Entry = Entry Time Activity deriving (Eq, Show)
type Section = Map Time Activity

parseActivity :: Parser String
parseActivity = do
  a <- try (manyTill (noneOf "\n") comment) <|> many (noneOf "\n") 
  skipOptional  skipLine
  return a

comment :: Parser String
comment = try (someSpace >> string "--") <|> string "--"

skipLine :: Parser ()
skipLine = skipMany (noneOf "\n") >> skipOptional (char '\n') >> return ()

parseDate :: Parser Time
parseDate = do
  string ">> "
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  let ym = read year * 525600
      mm = read month * 43800
      dm = read day * 1440
  return $ Time (ym + mm + dm)
      

parseEntry :: Parser Entry
parseEntry = do
  h <- count 2 digit
  char ':'
  m <- count 2 digit
  char ' '
  e <- parseActivity 
  let hm = read  h * 60
      mm = read  m
  return $ Entry (Time (hm + mm)) e

skipComment :: Parser ()
skipComment = skipOptional (comment >> skipLine)

parseSection :: Parser Section 
parseSection = do
  skipMany (noneOf ">>")
  d <- parseDate
  skipComment
  whiteSpace
  entries <- some parseEntry
  return  $ M.fromList $ readEntry d <$> entries

readEntry :: Time -> Entry -> (Time, Activity)
readEntry d (Entry t a) = (d + t, a)

parseLog :: Parser Section
parseLog = do
  xs <- some (M.toList <$> parseSection)
  return $ M.fromList . concat $ xs


instance Show Time where
  show (Time rawmin) = let
    ym = 525600
    mm = 43800
    dm = 1440
    hm = 60
    y = quot rawmin ym
    yr = rem rawmin ym
    mo = quot yr mm
    mr = rem yr mm
    d = quot mr dm
    dr = rem mr dm
    h = quot dr hm
    m = rem dr hm
    in printf "%04d" y ++ "-" ++
       printf "%02d" mo ++ "-" ++
       printf "%02d" d ++ " " ++
       printf "%02d" h ++ ":" ++
       printf "%02d" (abs m)

instance Fractional Time where
  fromRational m = Time (floor m)
  Time m / Time m' = fromRational (fromIntegral m / fromInteger m')

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

withinDay :: (Activity, Time) -> Bool
withinDay (_, t) = t < Time 1440 && t > Time 0

activityTime :: Result Section -> [(Activity, Time)]
activityTime (Success ms) =
  filter withinDay $ map  (timeSpent ms) (M.toList ms)
  where
    endTime k ms' =
      case M.lookupGT k ms' of
        Nothing -> k
        Just (t', _) -> t'
    timeSpent ms' (t, a) = (a, endTime t ms' - t)
activityTime _ = []

parsedLog :: Result Section
parsedLog = parseByteString parseLog mempty logEx


activitySum :: Map Activity Time
activitySum = M.fromListWith (+)
            $ activityTime parsedLog

extraDates :: Result Section -> [String]
extraDates (Success ms) =
  let xs = M.toList ms
  in map (\(t, _) -> take 10 $ show t) xs
extraDates _ = []

countDays :: [String] -> Integer
countDays xs =
  let unique = S.toList . S.fromList
  in genericLength $ unique xs

avgActTimePerDay :: Result Section -> Map Activity Time
avgActTimePerDay pLog =
  let days = countDays $ extraDates pLog
      sumTime = M.fromListWith (+) $ activityTime pLog
   in (/ fromInteger days) <$> sumTime








