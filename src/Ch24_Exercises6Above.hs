module Ch24_Exercises6Above where

import Data.Word
import Data.List (elemIndex)
import Text.Trifecta
import Data.Maybe
import Numeric
import Test.Hspec


newtype IPAddress = IPAddress Word32
 deriving (Eq, Ord, Show)

type Rem = Integer
type Bit = Integer
type Pos = Integer

data ParseState = ParseState
  { remainder :: Rem
  , bits :: [Bit]
  , posit :: Pos
  } deriving Show

spewBit :: ParseState -> ParseState
spewBit ps =
  let p' = posit ps - 1
      b = 2 ^ p' :: Pos
      (q, r) = remainder ps `quotRem` b
      xs = q : bits ps
   in ParseState {remainder = r, bits = xs, posit = p'}

spewPart :: ParseState -> [Bit]
spewPart ps =
  if posit ps == 0
  then reverse $ bits ps
  else spewPart $ spewBit ps


initState :: Pos -> Pos -> ParseState
initState p n = ParseState {remainder = n, bits = mempty, posit = p}


parseIPv4 :: Parser IPAddress
parseIPv4 = do
  n <- decimal
  char '.'
  n' <- decimal
  char '.'
  sn <- decimal
  char '.'
  h <- decimal
  let xs = concat ((spewPart <$> initState 8) . fromIntegral <$> [n, n', sn, h])
  return $ IPAddress (fromIntegral $ bitToIntegral (0, xs))


spewIntegral :: (Pos, [Bit]) -> (Pos, [Bit])
spewIntegral (s, xs) =
  let l = length xs - 1
      t = 2 ^ l
  in (s + head xs * t, tail xs)
  
bitToIntegral :: (Pos, [Bit]) -> Pos
bitToIntegral (i, xs) =
  if null xs
  then i
  else bitToIntegral $ spewIntegral (i, xs)

ipv4Main :: IO ()
ipv4Main =
  hspec $
  describe "Test some IP values" $ do
    let ip1 = "172.16.254.1"
        ip2 = "204.120.0.15"
    it ("can parse " ++ ip1) $ do
      let (Success x) = parseString parseIPv4 mempty ip1
      x `shouldBe` IPAddress 2886794753
    it ("can parse " ++ ip2) $ do
      let (Success x) = parseString parseIPv4 mempty ip2
      x `shouldBe` IPAddress 3430416399

data IPAddress6 = IPAddress6 Word64 Word64
  deriving (Eq, Ord)

instance Show IPAddress6 where
  show = show . ip6ToInteger

ip6ToInteger :: IPAddress6 -> Integer
ip6ToInteger (IPAddress6 q r) = toInteger q * toInteger (maxBound :: Word) + toInteger r

hex :: String
hex = ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']

parseBlock :: Parser String
parseBlock = many $ oneOf hex

parseBlocks :: Parser [String]
parseBlocks = sepBy1 parseBlock (char ':')

fillAbbrev :: [String] -> [String]
fillAbbrev xs =
  if "" `elem` xs
  then
    if length xs > 8
    then catMaybes $(\a -> if a == "" then Nothing else Just a) <$> xs
    else let i = fromJust $ elemIndex "" xs
             t = splitAt i xs
         in fillAbbrev (fst t ++ ["0"] ++ snd t)
  else xs

parseIPv6 :: Parser IPAddress6
parseIPv6 = do
  xs <- parseBlocks
  let xs' =
        concat
          ((spewPart <$> initState 16) . fst <$>
          catMaybes (listToMaybe . readHex <$> fillAbbrev xs))
      x = bitToIntegral (0, xs')
      ip = quotRem x $ fromIntegral (maxBound :: Word)
  return $ IPAddress6 (fromIntegral $ fst ip) (fromIntegral $ snd ip)
  

mainIPV6 :: IO ()
mainIPV6 =
  hspec $
  describe "Test some IP values" $ do
    let ip1 = "FE80::0202:B3FF:FE1E:8329"
        ip2 = "0:0:0:0:0:ffff:cc78:f"
    it ("can parse " ++ ip1) $ do
      let (Success x) = parseString parseIPv6 mempty ip1
          r = ip6ToInteger x
      r `shouldBe` 338288524927261089654163772891438416681
    it ("can parse " ++ ip2) $ do
      let (Success x) = parseString parseIPv6 mempty ip2
          r = ip6ToInteger x
      r `shouldBe` 281474112159759













