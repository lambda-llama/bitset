{-# LANGUAGE GADTs #-}

module Main (main) where

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
    let bs1 = BitSet.fromList elems1
        bs2 = BitSet.fromList elems2
        s1  = Set.fromList elems1
        s2  = Set.fromList elems2
    return $ rnf [B bs1, B bs2, B s1, B s2]
    defaultMain
        [ bgroup "Set"
          [ bench "fromList" (nf Set.fromList elems1)
          , bench "toList" (nf Set.toList s1)
          , bench "insert" (nf (insertS elems1) Set.empty)
          , bench "delete" (nf (deleteS elems1) s1)
          , bench "member" (nf (memberS elems1) s1)
          , bench "isSubsetOf" (nf (Set.isSubsetOf s2) s1)
          , bench "isProperSubsetOf" (nf (Set.isProperSubsetOf s2) s1)
          , bench "intersection" (nf (Set.intersection s2) s1)
          , bench "difference" (nf (Set.difference s2) s1)
          , bench "union" (nf (Set.union s2) s1)
          ]

        , bgroup "BitSet"
          [ bench "fromList" (nf BitSet.fromList elems1)
          , bench "toList" (nf BitSet.toList bs1)
          , bench "insert" (nf (insertBS elems1) BitSet.empty)
          , bench "delete" (nf (deleteBS elems1) bs1)
          , bench "member" (nf (memberBS elems1) bs1)
          , bench "isSubsetOf" (nf (BitSet.isSubsetOf bs2) bs1)
          , bench "isProperSubsetOf" (nf (BitSet.isProperSubsetOf bs2) bs1)
          , bench "intersection" (nf (BitSet.intersection bs2) bs1)
          , bench "difference" (nf (BitSet.difference bs2) bs1)
          , bench "union" (nf (BitSet.union bs2) bs1)
          ]
        ]
  where
    n :: Int
    n = 128

    elems1 = [1..n]
    elems2 = [1..n `div` 2]

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
