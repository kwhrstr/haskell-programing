module Ch27_CoreDump where

discriminatory :: Bool -> Int
discriminatory False = 0
discriminatory True = 1

discriminatory2 :: Bool -> Int
discriminatory2 b =
  let x = undefined
   in (if x `seq` b
         then 1
         else 0)

hypo'' :: IO ()
hypo'' = do
  let x :: Integer
      x = undefined
  s <- x `seq` getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "hello"
