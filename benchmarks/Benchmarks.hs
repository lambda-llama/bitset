{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Monad (void)
import Data.List (foldl')

import Control.DeepSeq (NFData(..))
import Criterion.Main (defaultMain, bench, bgroup, nf)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

main :: IO ()
main = do
    let bs = BitSet.fromList elems
        s  = Set.fromList elems
    return $ rnf [B bs, B s]
    defaultMain
        [ bgroup "Set"
          [ bench "fromList" (nf Set.fromList elems)
          , bench "toList" (nf Set.toList s)
          , bench "insert" (nf (insertS elems) Set.empty)
          , bench "delete" (nf (deleteS elems) s)
          , bench "member" (nf (memberS elems) s)
          ]

        , bgroup "BitSet"
          [ bench "fromList" (nf BitSet.fromList elems)
          , bench "toList" (nf BitSet.toList bs)
          , bench "insert" (nf (insertBS elems) BitSet.empty)
          , bench "delete" (nf (deleteBS elems) bs)
          , bench "member" (nf (memberBS elems) bs)
          ]
        ]
  where
    n :: Int
    n = 128

    elems = [1..n]

memberS :: [Int] -> Set Int -> Bool
memberS xs s = all (\x -> Set.member x s) xs

memberBS :: [Int] -> BitSet Int -> Bool
memberBS xs bs = all (\x -> BitSet.member x bs) xs

insertS :: [Int] -> Set Int -> Set Int
insertS xs s0 = foldl' (\s x -> Set.insert x s) s0 xs

insertBS :: [Int] -> BitSet Int -> BitSet Int
insertBS xs bs0 = foldl' (\bs x -> BitSet.insert x bs) bs0 xs

deleteS :: [Int] -> Set Int -> Set Int
deleteS xs s0 = foldl' (\s x -> Set.delete x s) s0 xs

deleteBS :: [Int] -> BitSet Int -> BitSet Int
deleteBS xs bs0 = foldl' (\bs x -> BitSet.delete x bs) bs0 xs
