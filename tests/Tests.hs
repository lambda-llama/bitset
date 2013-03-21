module Main (main) where

import Control.Applicative ((<$>))
import Data.List ((\\), intersect, union, nub)

import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary(..), (==>), classify)

import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet

instance (Arbitrary a, Enum a) => Arbitrary (BitSet a) where
    arbitrary = BitSet.fromList <$> arbitrary


propSize :: [Int] -> Bool
propSize = go . nub . map abs where
  go xs = length xs == BitSet.size (foldr BitSet.insert BitSet.empty xs)

propSizeAfterInsert :: Int -> BitSet Int -> Property
propSizeAfterInsert x bs =
    x >= 0 ==> BitSet.size (BitSet.insert x bs) == BitSet.size bs + diff
  where
    diff :: Int
    diff = if x `BitSet.member` bs then 0 else 1

propSizeAfterDelete :: Int -> BitSet Int -> Property
propSizeAfterDelete x bs =
    x >= 0 ==> BitSet.size (BitSet.delete x bs) == BitSet.size bs - diff
  where
    diff :: Int
    diff = if x `BitSet.member` bs then 1 else 0

propInsertMember :: Int -> BitSet Int -> Property
propInsertMember x bs = x >= 0 ==> x `BitSet.member` BitSet.insert x bs

propDeleteMember :: Int -> BitSet Int -> Property
propDeleteMember x bs = x >= 0 ==> x `BitSet.notMember` BitSet.delete x bs

propInsertDeleteIdempotent :: Int -> BitSet Int -> Property
propInsertDeleteIdempotent x bs = x `BitSet.notMember` bs && x >= 0 ==>
                                  bs == BitSet.delete x (BitSet.insert x bs)

propDeleteIdempotent :: Int -> BitSet Int -> Property
propDeleteIdempotent x bs = x >= 0 ==>
    classify (x `BitSet.member` bs) "x in bs" $
    classify (x `BitSet.notMember` bs) "x not in bs" $
    BitSet.delete x bs == BitSet.delete x (BitSet.delete x bs)

propInsertIdempotent :: Int -> BitSet Int -> Property
propInsertIdempotent x bs =
    x >= 0 ==> BitSet.insert x bs == BitSet.insert x (BitSet.insert x bs)

propFromList :: [Int] -> Property
propFromList xs = all (>= 0) xs ==> all (`BitSet.member` bs) xs where
  bs :: BitSet Int
  bs = BitSet.fromList xs

propEmpty :: Int -> Property
propEmpty x = x >= 0 ==> x `BitSet.notMember` BitSet.empty

propUnsafeFromIntegral :: Int -> Property
propUnsafeFromIntegral x =
    x >= 0 ==> bs == BitSet.unsafeFromIntegral (BitSet.toIntegral bs :: Integer)
  where
    bs :: BitSet Int
    bs = BitSet.singleton x

propUnions :: [Int] -> Property
propUnions xs =
    n >= 0 && all (>= 0) xs ==> all (`BitSet.member` bs) xs
  where
    n      = length xs
    (l, r) = splitAt (n `div` 2) xs

    bs :: BitSet Int
    bs = BitSet.unions $ map BitSet.fromList [l, r, l, r, l]

propIntersectionWithSelf :: [Int] -> Property
propIntersectionWithSelf xs =
    all (>= 0) xs ==> all (`BitSet.member` bs) xs
  where
    bs :: BitSet Int
    bs = let bs0 = BitSet.fromList xs in bs0 `BitSet.intersection` bs0

propIntersection :: [Int] -> Property
propIntersection xs = n >= 16 ==>
                       all (`BitSet.member` bs) (l `intersect` r) &&
                       all (`BitSet.notMember` bs) (dl `union` dr)
  where
    n      = length xs
    (l, r) = splitAt (n `div` 2) $ map abs xs

    dl = l \\ r
    dr = r \\ l

    bs :: BitSet Int
    bs = let bs1 = BitSet.fromList l
             bs2 = BitSet.fromList r
         in bs1 `BitSet.intersection` bs2

propDifferenceWithSelf :: [Int] -> Property
propDifferenceWithSelf xs = all (>= 0) xs ==> bs == BitSet.empty where
  bs :: BitSet Int
  bs = let bs0 = BitSet.fromList xs in bs0 `BitSet.difference` bs0

propDifference :: [Int] -> Property
propDifference xs = n >= 0 ==>
                    all (`BitSet.member` bs) (l \\ r) &&
                    all (`BitSet.notMember` bs) (l `intersect` r)
  where
    n      = length xs
    (l, r) = splitAt (n `div` 2) . nub $ map abs xs

    bs :: BitSet Int
    bs = let bs1 = BitSet.fromList l
             bs2 = BitSet.fromList r
         in bs1 `BitSet.difference` bs2

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
          , testProperty "fromList" propFromList
          , testProperty "empty" propEmpty
          , testProperty "unsafe construction from integral" propUnsafeFromIntegral
          , testProperty "unions" propUnions
          , testProperty "intersection with self" propIntersectionWithSelf
          , testProperty "intersection" propIntersection
          , testProperty "difference with self" propDifferenceWithSelf
          , testProperty "difference" propDifference
          ]
