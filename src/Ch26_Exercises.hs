
module Ch26_Exercises where
import Control.Monad.Trans.Reader
import Data.Functor.Identity 
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy (StateT(..))
import Control.Monad
import Control.Monad.Trans.Maybe

rDec :: Num a => Reader a a
rDec = reader $ \r -> r - 1

rDec' :: Num a => Reader a a
rDec' = reader $ flip (-) 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rShow' :: Show a => Reader a String
rShow' = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  print $ "hi: " ++ show r
  return $ r + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  let str = show s
  print $ "hi: " ++ str
  return (str, s + 1)

isValid :: String -> Bool
isValid  = elem '!'

maybeExcite :: MaybeT IO String
maybeExcite =  do
  v <- liftIO getLine
  guard $ isValid v
  return v
  
doExcite :: IO ()
doExcite = do
  putStrLn "say somet!hing excite"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MORE EXCITE"
    Just e -> putStrLn $ "God, was very excite: " ++ e

  