{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Control.Applicative ((<$>))
import Data.Bits (popCount, testBit, setBit, clearBit)
import Data.Int (Int16)
import Data.List ((\\), intersect, union, nub, sort)
import Data.Monoid ((<>), mempty)
import Data.Word (Word16)
import Foreign (Storable(..), allocaBytes)

import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, Arbitrary(..), (==>), classify)
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Data.BitSet (BitSet)
import Data.BitSet.Dynamic (FasterInteger(..))
import qualified Data.BitSet as BS
import qualified Data.BitSet.Generic as GS

instance (Arbitrary a, Enum a) => Arbitrary (BitSet a) where
    arbitrary = BS.fromList <$> arbitrary

instance (Arbitrary a, Enum a) => Arbitrary (GS.BitSet Word16 a) where
    arbitrary = GS.fromList <$> arbitrary

instance Show (Word16 -> a) where
    show = const "FUNCTION"

instance Arbitrary FasterInteger where
    arbitrary = FasterInteger <$> arbitrary

propSize :: [Word16] -> Bool
propSize = go . nub where
  go xs = length xs == BS.size (BS.fromList xs)

propSizeAfterInsert :: Word16 -> BitSet Word16 -> Bool
propSizeAfterInsert x bs =
    BS.size (BS.insert x bs) == BS.size bs + diff
  where
    diff :: Int
    diff = if x `BS.member` bs then 0 else 1

propSizeAfterDelete :: Word16 -> BitSet Word16 -> Bool
propSizeAfterDelete x bs =
    BS.size (BS.delete x bs) == BS.size bs - diff
  where
    diff :: Int
    diff = if x `BS.member` bs then 1 else 0

propInsertMember :: Word16 -> BitSet Word16 -> Bool
propInsertMember x bs = x `BS.member` BS.insert x bs

propDeleteMember :: Word16 -> BitSet Word16 -> Bool
propDeleteMember x bs = x `BS.notMember` BS.delete x bs

propInsertDeleteIdempotent :: Word16 -> BitSet Word16 -> Property
propInsertDeleteIdempotent x bs =
    x `BS.notMember` bs ==>
    bs == BS.delete x (BS.insert x bs)

propDeleteIdempotent :: Word16 -> BitSet Word16 -> Property
propDeleteIdempotent x bs =
    classify (x `BS.member` bs) "x in bs" $
    classify (x `BS.notMember` bs) "x not in bs" $
    BS.delete x bs == BS.delete x (BS.delete x bs)

propInsertIdempotent :: Word16 -> BitSet Word16 -> Bool
propInsertIdempotent x bs =
    BS.insert x bs == BS.insert x (BS.insert x bs)

propToList :: [Word16] -> Bool
propToList xs = nub (sort xs) == BS.toList bs where
  bs :: BitSet Word16
  bs = BS.fromList xs

propFromList :: [Word16] -> Bool
propFromList xs = all (`BS.member` bs) xs where
  bs :: BitSet Word16
  bs = BS.fromList xs

propEmpty :: Word16 -> Bool
propEmpty x = x `BS.notMember` BS.empty

propNullEmpty :: Bool
propNullEmpty = BS.null bs where
  bs :: BitSet Word16
  bs = BS.empty

propNullAfterDelete :: [Word16] -> Bool
propNullAfterDelete xs = BS.null bs where
  bs :: BitSet Word16
  bs = foldr BS.delete (foldr BS.insert BS.empty xs) xs

propIntersectionWithSelf :: [Word16] -> Bool
propIntersectionWithSelf xs = all (`BS.member` bs) xs
  where
    bs :: BitSet Word16
    bs = let bs0 = BS.fromList xs in
         bs0 `BS.intersection` bs0

propIntersection :: [Word16] -> Bool
propIntersection xs =
    all (`BS.member` bs) (l `intersect` r) &&
    all (`BS.notMember` bs) (dl `union` dr)
  where
    n      = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    dl = l \\ r
    dr = r \\ l

    bs :: BitSet Word16
    bs = let bs1 = BS.fromList l
             bs2 = BS.fromList r
         in bs1 `BS.intersection` bs2

propDifferenceWithSelf :: [Word16] -> Bool
propDifferenceWithSelf xs = bs == BS.empty where
  bs :: BitSet Word16
  bs = let bs0 = BS.fromList xs in
       bs0 `BS.difference` bs0

propDifference :: [Word16] -> Property
propDifference xs = n > 0 ==>
                    all (`BS.member` bs) (l \\ r) &&
                    all (`BS.notMember` bs) (l `intersect` r)
  where
    n      = length xs
    (l, r) = splitAt (n `div` 2) $ nub xs

    bs :: BitSet Word16
    bs = let bs1 = BS.fromList l
             bs2 = BS.fromList r
         in bs1 `BS.difference` bs2

propMonoidLaws :: BitSet Word16 -> BitSet Word16 -> BitSet Word16 -> Bool
propMonoidLaws bs1 bs2 bs3 =
    bs1 <> mempty == bs1 &&
    mempty <> bs1 == bs1 &&
    bs1 <> (bs2 <> bs3) == (bs1 <> bs2) <> bs3

propIsSubsetOfSelf :: BitSet Word16 -> Bool
propIsSubsetOfSelf bs = bs `BS.isSubsetOf` bs &&
                        not (bs `BS.isProperSubsetOf` bs)

propIsSubsetOf :: [Word16] -> Bool
propIsSubsetOf xs =
    bs1 `BS.isSubsetOf` bs &&
    bs2 `BS.isSubsetOf` bs
  where
    n = length xs

    bs :: BitSet Word16
    bs = BS.fromList xs

    bs1 :: BitSet Word16
    bs1 = BS.fromList $ take (n `div` 2) xs

    bs2 :: BitSet Word16
    bs2 = BS.fromList $ drop (n `div` 2) xs

propShowRead :: BitSet Word16 -> Bool
propShowRead bs = bs == (read $ show bs)

propMap :: BitSet Word16 -> (Word16 -> Word16) -> Bool
propMap bs f = BS.map f bs == (BS.fromList $ map f $ BS.toList bs)

propFilter :: BitSet Word16 -> (Word16 -> Bool) -> Bool
propFilter bs f = BS.filter f bs == (BS.fromList $ filter f $ BS.toList bs)

propStorable :: GS.BitSet Word16 Word16 -> Property
propStorable storable = monadicIO $ do
    peeked <- run $ do
        allocaBytes size $ \ptr -> do
            poke ptr storable
            peek ptr
    assert $ storable == peeked
  where
    size = sizeOf storable


#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 706)
propPopCount :: FasterInteger -> Property
propPopCount xfi = xfi >= 0 ==> popCount xfi == popCount xi where
  xi :: Integer
  xi = fromIntegral xfi
#endif

propTestBit :: FasterInteger -> Int16 -> Property
propTestBit xfi i = xfi >= 0 ==> testBit xfi bit == testBit xi bit where
  bit :: Int
  bit = fromIntegral i

  xi :: Integer
  xi = fromIntegral xfi

propSetBit :: FasterInteger -> Int16 -> Property
propSetBit xfi i =
    xfi >= 0 ==> setBit xfi bit == FasterInteger (setBit xi bit)
  where
    bit :: Int
    bit = fromIntegral i

    xi :: Integer
    xi = fromIntegral xfi

propClearBit :: FasterInteger -> Int16 -> Property
propClearBit xfi i =
    xfi >= 0 ==>
    classify True "x not in bs" $
    clearBit xfi bit == FasterInteger (clearBit xi bit)

  where
    bit :: Int
    bit = fromIntegral i

    xi :: Integer
    xi = fromIntegral xfi


main :: IO ()
main = defaultMain tests where
  tests :: [Test]
  tests = [ testGroup "Data.BitSet" testsBitSet
          , testGroup "GHC.Integer.GMP" testsFasterInteger
          ]

  testsBitSet :: [Test]
  testsBitSet =
      [ testProperty "size" propSize
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
      , testProperty "native empty is null" propNullEmpty
      , testProperty "generated empty is null" propNullAfterDelete
      , testProperty "intersection with self" propIntersectionWithSelf
      , testProperty "intersection" propIntersection
      , testProperty "difference with self" propDifferenceWithSelf
      , testProperty "difference" propDifference
      , testProperty "monoid laws" propMonoidLaws
      , testProperty "is subset of self" propIsSubsetOfSelf
      , testProperty "is subset of" propIsSubsetOf
      , testProperty "show read" propShowRead
      , testProperty "map" propMap
      , testProperty "filter" propFilter
      , testProperty "storable instance" propStorable
      ]

  testsFasterInteger :: [Test]
  testsFasterInteger =
      [
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 706)
        testProperty "pop count" propPopCount
      ,
#endif
        testProperty "test bit" propTestBit
      , testProperty "set bit" propSetBit
      , testProperty "clear bit" propClearBit
      ]
