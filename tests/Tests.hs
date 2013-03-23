{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Control.Applicative ((<$>))
import Data.List ((\\), intersect, union, nub, sort)
import Data.Monoid ((<>), mempty)
import Data.Word (Word16)

import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary(..), (==>), classify)

import Data.BitSet.Dynamic (BitSet)
import qualified Data.BitSet.Dynamic as DBS

instance (Arbitrary a, Enum a) => Arbitrary (BitSet a) where
    arbitrary = DBS.fromList <$> arbitrary


propSize :: [Word16] -> Bool
propSize = go . nub where
  go xs = length xs == DBS.size (DBS.fromList xs)

propSizeAfterInsert :: Word16 -> BitSet Word16 -> Bool
propSizeAfterInsert x bs =
    DBS.size (DBS.insert x bs) == DBS.size bs + diff
  where
    diff :: Int
    diff = if x `DBS.member` bs then 0 else 1

propSizeAfterDelete :: Word16 -> BitSet Word16 -> Bool
propSizeAfterDelete x bs =
    DBS.size (DBS.delete x bs) == DBS.size bs - diff
  where
    diff :: Int
    diff = if x `DBS.member` bs then 1 else 0

propInsertMember :: Word16 -> BitSet Word16 -> Bool
propInsertMember x bs = x `DBS.member` DBS.insert x bs

propDeleteMember :: Word16 -> BitSet Word16 -> Bool
propDeleteMember x bs = x `DBS.notMember` DBS.delete x bs

propInsertDeleteIdempotent :: Word16 -> BitSet Word16 -> Property
propInsertDeleteIdempotent x bs =
    x `DBS.notMember` bs ==>
    bs == DBS.delete x (DBS.insert x bs)

propDeleteIdempotent :: Word16 -> BitSet Word16 -> Property
propDeleteIdempotent x bs =
    classify (x `DBS.member` bs) "x in bs" $
    classify (x `DBS.notMember` bs) "x not in bs" $
    DBS.delete x bs == DBS.delete x (DBS.delete x bs)

propInsertIdempotent :: Word16 -> BitSet Word16 -> Bool
propInsertIdempotent x bs =
    DBS.insert x bs == DBS.insert x (DBS.insert x bs)

propToList :: [Word16] -> Bool
propToList xs = nub (sort xs) == DBS.toList bs where
  bs :: BitSet Word16
  bs = DBS.fromList xs

propFromList :: [Word16] -> Bool
propFromList xs = all (`DBS.member` bs) xs where
  bs :: BitSet Word16
  bs = DBS.fromList xs

propEmpty :: Word16 -> Bool
propEmpty x = x `DBS.notMember` DBS.empty

propUnions :: [Word16] -> Bool
propUnions xs = all (`DBS.member` bs) xs where
  n      = length xs
  (l, r) = splitAt (n `div` 2) xs

  bs :: BitSet Word16
  bs = DBS.unions $ map DBS.fromList [l, r, l, r, l]

propIntersectionWithSelf :: [Word16] -> Bool
propIntersectionWithSelf xs = all (`DBS.member` bs) xs
  where
    bs :: BitSet Word16
    bs = let bs0 = DBS.fromList xs in
         bs0 `DBS.intersection` bs0

propIntersection :: [Word16] -> Bool
propIntersection xs =
    all (`DBS.member` bs) (l `intersect` r) &&
    all (`DBS.notMember` bs) (dl `union` dr)
  where
    n      = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    dl = l \\ r
    dr = r \\ l

    bs :: BitSet Word16
    bs = let bs1 = DBS.fromList l
             bs2 = DBS.fromList r
         in bs1 `DBS.intersection` bs2

propDifferenceWithSelf :: [Word16] -> Bool
propDifferenceWithSelf xs = bs == DBS.empty where
  bs :: BitSet Word16
  bs = let bs0 = DBS.fromList xs in
       bs0 `DBS.difference` bs0

propDifference :: [Word16] -> Property
propDifference xs = n > 0 ==>
                    all (`DBS.member` bs) (l \\ r) &&
                    all (`DBS.notMember` bs) (l `intersect` r)
  where
    n      = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    bs :: BitSet Word16
    bs = let bs1 = DBS.fromList l
             bs2 = DBS.fromList r
         in bs1 `DBS.difference` bs2

propMonoidLaws :: BitSet Word16 -> BitSet Word16 -> BitSet Word16 -> Bool
propMonoidLaws bs1 bs2 bs3 =
    bs1 <> mempty == bs1 &&
    mempty <> bs1 == bs1 &&
    bs1 <> (bs2 <> bs3) == (bs1 <> bs2) <> bs3

propIsSubsetOfSelf :: BitSet Word16 -> Bool
propIsSubsetOfSelf bs = bs `DBS.isSubsetOf` bs &&
                        not (bs `DBS.isProperSubsetOf` bs)

propIsSubsetOf :: [Word16] -> Bool
propIsSubsetOf xs =
    bs1 `DBS.isSubsetOf` bs &&
    bs2 `DBS.isSubsetOf` bs
  where
    n = length xs

    bs :: BitSet Word16
    bs = DBS.fromList xs

    bs1 :: BitSet Word16
    bs1 = DBS.fromList $ take (n `div` 2) xs

    bs2 :: BitSet Word16
    bs2 = DBS.fromList $ drop (n `div` 2) xs

main :: IO ()
main = defaultMain tests where
  tests :: [Test]
  tests = [ testProperty "size" propSize
          , testProperty "size after insert" propSizeAfterInsert
          , testProperty "size after delete" propSizeAfterDelete
          , testProperty "insert" propInsertMember
          , testProperty "delete" propDeleteMember
          , testProperty "insert and delete are idempotent" propInsertDeleteIdempotent
          , testProperty "delete is idempotent" propDeleteIdempotent
          , testProperty "insert is idempotent" propInsertIdempotent
          , testProperty "toList" propToList
          , testProperty "fromList" propFromList
          , testProperty "empty" propEmpty
          , testProperty "unions" propUnions
          , testProperty "intersection with self" propIntersectionWithSelf
          , testProperty "intersection" propIntersection
          , testProperty "difference with self" propDifferenceWithSelf
          , testProperty "difference" propDifference
          , testProperty "monoid laws" propMonoidLaws
          , testProperty "is subset of self" propIsSubsetOfSelf
          , testProperty "is subset of" propIsSubsetOf
          ]
