{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitSet
-- Copyright   :  (c) Sergei Lebedev 2013.
--                Based on Data.BitSet (c) Denis Bueno 2008-2009
-- License     :  MIT
-- Maintainer  :  superbobry@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable

-- A /bit set/ maintains a record of members from a type that can be
-- enumerated (i. e. has and `Enum' instance). The maximum number of elements
-- that can be stored is @maxBound :: Int@.
--
-- To use this library, define a `Enum' instance for your data type or
-- have it derived. It is important that the values you intend to store
-- in a bit set start from 0 and go up. A value for which @fromEnum x@ is @n@
-- corresponds to bit location @n@ in an @Integer@, and thus requires that
-- @Integer@ to have at least @n@ bits.
module Data.BitSet
    (
    -- * Bit set type
      BitSet

    -- * Operators
    , (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , isSubsetOf
    , isProperSubsetOf

    -- * Construction
    , empty
    , singleton
    , insert
    , delete

    -- * Combine
    , union
    , unions
    , difference
    , intersection

    -- * Conversion
    -- ** List
    , elems
    , toList
    , fromList
    -- ** Arbitraty integral type
    , toIntegral
    , unsafeFromIntegral
    ) where

import Prelude hiding (null)

import Data.Bits (Bits, (.|.), (.&.), complement, bit,
                  testBit, setBit, clearBit, shiftR, popCount)
import Data.Data (Typeable)
import Data.Foldable (Foldable(foldMap), toList)
import Data.List (foldl')
import Data.Monoid (Monoid(..))

import Control.DeepSeq (NFData(..))

data BitSet a = Enum a => BitSet {-# UNPACK #-} !Int !Integer
    deriving Typeable

instance Eq (BitSet a) where
    BitSet n1 i1 == BitSet n2 i2 = n1 == n2 && i1 == i2

instance Ord (BitSet a) where
    BitSet n1 i1 `compare` BitSet n2 i2 = case n1 `compare` n2 of
        EQ -> i1 `compare` i2
        r  -> r

instance Enum a => Monoid (BitSet a) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance Show a => Show (BitSet a) where
    show bs = "fromList " ++ show (elems bs)

instance NFData (BitSet a) where
    rnf (BitSet n i) = rnf n `seq` rnf i `seq` ()

instance Foldable BitSet where
    foldMap f (BitSet _n i0) = go 0 i0 where
        go _b 0 = mempty
        go b i  =
            if i `testBit` 0
            then f (toEnum b) `mappend` go (b + 1) (shiftR i 1)
            else go (b + 1) (shiftR i 1)

-- | /O(1)/. Is the bit set empty?
null :: BitSet a -> Bool
null (BitSet _n i) = i == 0
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the bit set.
size :: BitSet a -> Int
size (BitSet n _i) = n
{-# INLINE size #-}

-- | /O(testBit on Integer)/. Ask whether the item is in the bit set.
member :: a -> BitSet a -> Bool
member x (BitSet _n i) = testBit i (fromEnum x)
{-# INLINE member #-}

-- | /O(testBit on Integer)/. Ask whether the item is in the bit set.
notMember :: a -> BitSet a -> Bool
notMember bs = not . member bs
{-# INLINE notMember #-}

-- | /O(max(n, m))/. Is this a subset? (@s1 isSubsetOf s2@) tells whether
-- @s1@ is a subset of @s2@.
isSubsetOf :: BitSet a -> BitSet a -> Bool
isSubsetOf (BitSet n1 i1) (BitSet n2 i2) = n2 >= n1 && i2 .|. i1 == i2

-- | /O(max(n, m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: BitSet a -> BitSet a -> Bool
isProperSubsetOf bs1 bs2 = bs1 `isSubsetOf` bs2 && bs1 /= bs2

-- | The empty bit set.
empty :: Enum a => BitSet a
empty = BitSet 0 0
{-# INLINE empty #-}

-- | O(setBit on Integer). Create a singleton set.
singleton :: Enum a => a -> BitSet a
singleton x = BitSet 1 $! bit (fromEnum x)
{-# INLINE singleton #-}

-- | /O(setBit on Integer)/. Insert an item into the bit set.
insert :: a -> BitSet a -> BitSet a
insert x (BitSet n i) = BitSet n' $ setBit i e where
  n' = if testBit i e then n else n + 1
  e  = fromEnum x
{-# INLINE insert #-}

-- | /O(clearBit on Integer)/. Delete an item from the bit set.
delete :: a -> BitSet a -> BitSet a
delete x (BitSet n i) = BitSet n' $ clearBit i e where
  n' = if testBit i e then n - 1 else n
  e  = fromEnum x
{-# INLINE delete #-}

-- | /O(max(m, n))/. The union of two bit sets.
union :: BitSet a -> BitSet a -> BitSet a
union (BitSet _n1 i1) (BitSet _n2 i2) = BitSet (popCount i) i where
  i = i1 .|. i2
{-# INLINE union #-}

-- | /O(max(m, n))/. The union of a list of bit sets.
unions :: Enum a => [BitSet a] -> BitSet a
unions = foldl' union empty
{-# INLINE unions #-}

-- | /O(max(m, n))/. Difference of two bit sets.
difference :: BitSet a -> BitSet a -> BitSet a
difference (BitSet _n1 i1) (BitSet _n2 i2) = BitSet (popCount i) i where
  i = i1 .&. complement i2

-- | /O(max(m, n))/. See `difference'.
(\\) :: BitSet a -> BitSet a -> BitSet a
(\\) = difference

-- | /O(max(m, n))/. The intersection of two bit sets.
intersection :: BitSet a -> BitSet a -> BitSet a
intersection (BitSet _n1 i1) (BitSet _n2 i2) = BitSet (popCount i) i where
  i = i1 .&. i2

-- | /O(n * shiftR on Integer)/. An alias to @toList@.
elems :: BitSet a -> [a]
elems = toList

-- | /O(n * setBit on Integer)/. Make a bit set from a list of elements.
fromList :: Enum a => [a] -> BitSet a
fromList xs = BitSet (popCount i) i where
  i = foldl' (\b x -> setBit b (fromEnum x)) 0 xs

-- | /O(1)/. Project a bit set to an integral type.
toIntegral :: Integral b => BitSet a -> b
toIntegral (BitSet _n i) = fromIntegral i
{-# INLINE toIntegral #-}

-- | /O(n)/. Make a bit set from an integral. Unsafe because we don't
-- checked whether the bits set in a given value correspond to values
-- of type @a@. This is only useful as a more efficient alternative to
-- fromList.
unsafeFromIntegral :: (Enum a, Integral b) => b -> BitSet a
unsafeFromIntegral x = let i = fromIntegral x in BitSet (popCount i) i
{-# INLINE unsafeFromIntegral #-}
