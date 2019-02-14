module Ch28.Seq2 where

import Criterion.Main
import qualified Data.Sequence as S
lists :: [Int]
lists = [1..100000]

seqs :: S.Seq Int
seqs = S.fromList [1..100000]

main :: IO ()
main =
  defaultMain
    [ bench "indexing list" $ whnf (!! 9001) lists
    , bench "indexing sequence" $ whnf (`S.index` 9001) seqs
    ]