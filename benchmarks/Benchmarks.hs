{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import Data.List (foldl')

import Control.DeepSeq (NFData(..))
import Criterion.Main (defaultMain, bench, bgroup, nf)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.BitSet (BitSet)
import qualified Data.BitSet as BS

data B = forall a. NFData a => B a

instance NFData B where
    rnf (B b) = rnf b

main :: IO ()
main = do
    let bs1 = BS.fromList elems1
        bs2 = BS.fromList elems2
        s1  = Set.fromList elems1
        s2  = Set.fromList elems2
    return $ rnf [B bs1, B bs2, B s1, B s2]
    defaultMain
        [ bgroup "Set"
          [ bench "fromList" (nf Set.fromList elems1)
          , bench "toList" (nf Set.toList s1)
          , bench "singleton" (nf Set.singleton n)
          , bench "insert" (nf (insertS elems1) Set.empty)
          , bench "delete" (nf (deleteS elems1) s1)
          , bench "member" (nf (memberS elems1) s1)
          , bench "notMember" (nf (notMemberS elems2) s1)
          , bench "isSubsetOf" (nf (Set.isSubsetOf s2) s1)
          , bench "isProperSubsetOf" (nf (Set.isProperSubsetOf s2) s1)
          , bench "intersection" (nf (Set.intersection s2) s1)
          , bench "difference" (nf (Set.difference s2) s1)
          , bench "union" (nf (Set.union s2) s1)
          , bench "map" (nf (Set.map id) s1)
          , bench "filter" (nf (Set.filter $ const True) s1)
          ]

        , bgroup "BitSet"
          [ bench "fromList" (nf BS.fromList elems1)
          , bench "toList" (nf BS.toList bs1)
          , bench "singleton" (nf BS.singleton n)
          , bench "insert" (nf (insertBS elems1) BS.empty)
          , bench "delete" (nf (deleteBS elems1) bs1)
          , bench "member" (nf (memberBS elems1) bs1)
          , bench "notMember" (nf (notMemberBS elems2) bs1)
          , bench "isSubsetOf" (nf (BS.isSubsetOf bs2) bs1)
          , bench "isProperSubsetOf" (nf (BS.isProperSubsetOf bs2) bs1)
          , bench "intersection" (nf (BS.intersection bs2) bs1)
          , bench "difference" (nf (BS.difference bs2) bs1)
          , bench "union" (nf (BS.union bs2) bs1)
          , bench "map" (nf (BS.map id) bs1)
          , bench "filter" (nf (BS.filter $ const True) bs1)
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
memberBS xs bs = all (\x -> BS.member x bs) xs

notMemberS :: [Int] -> Set Int -> Bool
notMemberS xs s = all (\x -> Set.notMember x s) xs

notMemberBS :: [Int] -> BitSet Int -> Bool
notMemberBS xs bs = all (\x -> BS.notMember x bs) xs

insertS :: [Int] -> Set Int -> Set Int
insertS xs s0 = foldl' (\s x -> Set.insert x s) s0 xs

insertBS :: [Int] -> BitSet Int -> BitSet Int
insertBS xs bs0 = foldl' (\bs x -> BS.insert x bs) bs0 xs

deleteS :: [Int] -> Set Int -> Set Int
deleteS xs s0 = foldl' (\s x -> Set.delete x s) s0 xs

deleteBS :: [Int] -> BitSet Int -> BitSet Int
deleteBS xs bs0 = foldl' (\bs x -> BS.delete x bs) bs0 xs
