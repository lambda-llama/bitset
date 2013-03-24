{-# LANGUAGE ExistentialQuantification #-}

module Main (main) where

import Data.List (foldl')

import Control.DeepSeq (NFData(..))
import Criterion.Main (defaultMain, bench, bgroup, nf)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.BitSet.Dynamic (BitSet)
import qualified Data.BitSet.Dynamic as DBS

data B = forall a. NFData a => B a

instance NFData B where
    rnf (B b) = rnf b

main :: IO ()
main = do
    let bs1 = DBS.fromList elems1
        bs2 = DBS.fromList elems2
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
          ]

        , bgroup "BitSet"
          [ bench "fromList" (nf DBS.fromList elems1)
          , bench "toList" (nf DBS.toList bs1)
          , bench "singleton" (nf DBS.singleton n)
          , bench "insert" (nf (insertBS elems1) DBS.empty)
          , bench "delete" (nf (deleteBS elems1) bs1)
          , bench "member" (nf (memberBS elems1) bs1)
          , bench "notMember" (nf (notMemberBS elems2) bs1)
          , bench "isSubsetOf" (nf (DBS.isSubsetOf bs2) bs1)
          , bench "isProperSubsetOf" (nf (DBS.isProperSubsetOf bs2) bs1)
          , bench "intersection" (nf (DBS.intersection bs2) bs1)
          , bench "difference" (nf (DBS.difference bs2) bs1)
          , bench "union" (nf (DBS.union bs2) bs1)
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
memberBS xs bs = all (\x -> DBS.member x bs) xs

notMemberS :: [Int] -> Set Int -> Bool
notMemberS xs s = all (\x -> Set.notMember x s) xs

notMemberBS :: [Int] -> BitSet Int -> Bool
notMemberBS xs bs = all (\x -> DBS.notMember x bs) xs

insertS :: [Int] -> Set Int -> Set Int
insertS xs s0 = foldl' (\s x -> Set.insert x s) s0 xs

insertBS :: [Int] -> BitSet Int -> BitSet Int
insertBS xs bs0 = foldl' (\bs x -> DBS.insert x bs) bs0 xs

deleteS :: [Int] -> Set Int -> Set Int
deleteS xs s0 = foldl' (\s x -> Set.delete x s) s0 xs

deleteBS :: [Int] -> BitSet Int -> BitSet Int
deleteBS xs bs0 = foldl' (\bs x -> DBS.delete x bs) bs0 xs
