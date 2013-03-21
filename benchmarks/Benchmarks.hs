module Main (main) where

import Criterion.Main (defaultMain, bench, nf)

import qualified Data.BitSet as BitSet

main :: IO ()
main = do
    defaultMain
        [ bench "fromList" (nf BitSet.fromList [1..n])
        ]
  where
    n :: Int
    n = 1024
