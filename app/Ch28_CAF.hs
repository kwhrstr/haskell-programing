module Main where

incdInts :: [Integer] -> [Integer]
incdInts = map (+ 1)

main :: IO ()
main = print (incdInts [1 ..] !! 1000)
