module Main (main) where

import Control.Applicative ((<$>))
import Data.List ((\\), intersect, union, nub, sort)
import Data.Monoid (mempty, mappend)
import Data.Word (Word16)

import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary(..), (==>), classify)

import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet

instance (Arbitrary a, Enum a) => Arbitrary (BitSet a) where
    arbitrary = BitSet.fromList <$> arbitrary


propSize :: [Word16] -> Bool
propSize = go . nub where
  go xs = length xs == BitSet.size (BitSet.fromList xs)

propSizeAfterInsert :: Word16 -> BitSet Word16 -> Bool
propSizeAfterInsert x bs =
    BitSet.size (BitSet.insert x bs) == BitSet.size bs + diff
  where
    diff :: Int
    diff = if x `BitSet.member` bs then 0 else 1

propSizeAfterDelete :: Word16 -> BitSet Word16 -> Bool
propSizeAfterDelete x bs =
    BitSet.size (BitSet.delete x bs) == BitSet.size bs - diff
  where
    diff :: Int
    diff = if x `BitSet.member` bs then 1 else 0

propInsertMember :: Word16 -> BitSet Word16 -> Bool
propInsertMember x bs = x `BitSet.member` BitSet.insert x bs

propDeleteMember :: Word16 -> BitSet Word16 -> Bool
propDeleteMember x bs = x `BitSet.notMember` BitSet.delete x bs

propInsertDeleteIdempotent :: Word16 -> BitSet Word16 -> Property
propInsertDeleteIdempotent x bs =
    x `BitSet.notMember` bs ==> bs == BitSet.delete x (BitSet.insert x bs)

propDeleteIdempotent :: Word16 -> BitSet Word16 -> Property
propDeleteIdempotent x bs =
    classify (x `BitSet.member` bs) "x in bs" $
    classify (x `BitSet.notMember` bs) "x not in bs" $
    BitSet.delete x bs == BitSet.delete x (BitSet.delete x bs)

propInsertIdempotent :: Word16 -> BitSet Word16 -> Bool
propInsertIdempotent x bs =
    BitSet.insert x bs == BitSet.insert x (BitSet.insert x bs)

propToList :: [Word16] -> Bool
propToList xs = nub (sort xs) == BitSet.toList bs where
  bs :: BitSet Word16
  bs = BitSet.fromList xs

propFromList :: [Word16] -> Bool
propFromList xs = all (`BitSet.member` bs) xs where
  bs :: BitSet Word16
  bs = BitSet.fromList xs

propEmpty :: Word16 -> Bool
propEmpty x = x `BitSet.notMember` BitSet.empty

propUnsafeFromIntegral :: Word16 -> Bool
propUnsafeFromIntegral x =
    bs == BitSet.unsafeFromIntegral (BitSet.toIntegral bs :: Integer)
  where
    bs :: BitSet Word16
    bs = BitSet.singleton x

propUnions :: [Word16] -> Bool
propUnions xs = all (`BitSet.member` bs) xs where
  n      = length xs
  (l, r) = splitAt (n `div` 2) xs

  bs :: BitSet Word16
  bs = BitSet.unions $ map BitSet.fromList [l, r, l, r, l]

propIntersectionWithSelf :: [Word16] -> Bool
propIntersectionWithSelf xs = all (`BitSet.member` bs) xs
  where
    bs :: BitSet Word16
    bs = let bs0 = BitSet.fromList xs in bs0 `BitSet.intersection` bs0

propIntersection :: [Word16] -> Bool
propIntersection xs =
    all (`BitSet.member` bs) (l `intersect` r) &&
    all (`BitSet.notMember` bs) (dl `union` dr)
  where
    n      = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    dl = l \\ r
    dr = r \\ l

    bs :: BitSet Word16
    bs = let bs1 = BitSet.fromList l
             bs2 = BitSet.fromList r
         in bs1 `BitSet.intersection` bs2

propDifferenceWithSelf :: [Word16] -> Bool
propDifferenceWithSelf xs = bs == BitSet.empty where
  bs :: BitSet Word16
  bs = let bs0 = BitSet.fromList xs in bs0 `BitSet.difference` bs0

propDifference :: [Word16] -> Property
propDifference xs = n > 0 ==>
                    all (`BitSet.member` bs) (l \\ r) &&
                    all (`BitSet.notMember` bs) (l `intersect` r)
  where
    n      = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    bs :: BitSet Word16
    bs = let bs1 = BitSet.fromList l
             bs2 = BitSet.fromList r
         in bs1 `BitSet.difference` bs2

propMonoidLaws :: BitSet Word16 -> BitSet Word16 -> BitSet Word16 -> Bool
propMonoidLaws bs1 bs2 bs3 =
    bs1 `mappend` mempty == bs1 &&
    mempty `mappend` bs1 == bs1 &&
    mappend bs1 (bs2 `mappend` bs3) == (bs1 `mappend` bs2) `mappend` bs3

propIsSubsetOfSelf :: BitSet Word16 -> Bool
propIsSubsetOfSelf bs = bs `BitSet.isSubsetOf` bs &&
                        not (bs `BitSet.isProperSubsetOf` bs)

propIsSubsetOf :: [Word16] -> Bool
propIsSubsetOf xs =
    bs1 `BitSet.isSubsetOf` bs && bs2 `BitSet.isSubsetOf` bs
  where
    n = length xs

    bs :: BitSet Word16
    bs = BitSet.fromList xs

    bs1 :: BitSet Word16
    bs1 = BitSet.fromList $ take (n `div` 2) xs

    bs2 :: BitSet Word16
    bs2 = BitSet.fromList $ drop (n `div` 2) xs

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
          , testProperty "unsafe construction from integral" propUnsafeFromIntegral
          , testProperty "unions" propUnions
          , testProperty "intersection with self" propIntersectionWithSelf
          , testProperty "intersection" propIntersection
          , testProperty "difference with self" propDifferenceWithSelf
          , testProperty "difference" propDifference
          , testProperty "monoid laws" propMonoidLaws
          , testProperty "is subset of self" propIsSubsetOfSelf
          , testProperty "is subset of" propIsSubsetOf
          ]
