module Ch28.Criterion3 where


import Criterion.Main

myList :: [Int]
myList = [1..9999]

main :: IO ()
main = defaultMain
  [ bench "map list 9999" $
    whnf (map (+1)) myList
  ]