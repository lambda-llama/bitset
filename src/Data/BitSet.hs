{-# LANGUAGE DeriveDataTypeable #-}

-- | A /bit set/ maintains a record of members from a type that can be mapped
-- into (non-negative) @Int@s. The maximum number of elements that can be
-- stored is @maxbound :: Int@. Supports insertion, deletion, size, and
-- membership testing, and is completely pure (functional).
--
-- To use this library, simply supply a `Enum' instance for your data type or
-- have it derived. It is important that the values you intend to keep track
-- of start from 0 and go up. A value for which @fromEnum x@ is @n@
-- corresponds to bit location @n@ in an @Integer@, and thus requires that
-- @Integer@ to have at least @n@ bits.
--
-- The implementation is quite simple: we rely on the @Bits Integer@ instance
-- from @Data.Bits@.  An advantage of this library over simply using that
-- @Bits@ instance is the phantom type parameter used in the `BitSet' type.
-- The interface we expose ensures client code will not typecheck if it
-- confuses two bit sets intended to keep track of different types.
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

import Data.Bits (Bits, (.|.), (.&.), complement,
                  testBit, setBit, clearBit, shiftR, popCount)
import Data.Data (Data, Typeable)
import Data.List (foldl')
import Data.Monoid (Monoid(..))

import Control.DeepSeq (NFData(..))

data BitSet a = BitSet {-# UNPACK #-} !Int !Integer
    deriving (Eq, Ord, Data, Typeable)

instance Enum a => Monoid (BitSet a) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance (Enum a, Show a) => Show (BitSet a) where
    show bs = "fromList " ++ show (elems bs)

instance NFData (BitSet a) where
    rnf (BitSet count i) = rnf count `seq` rnf i `seq` ()


-- | /O(1)/ Is the bit set empty?
null :: BitSet a -> Bool
null (BitSet _n i) = i == 0
{-# INLINE null #-}

-- | /O(1)/ The number of elements in the bit set.
size :: BitSet a -> Int
size (BitSet n _i) = n
{-# INLINE size #-}

-- | /O(testBit on Integer)/ Ask whether the item is in the bit set.
member :: Enum a => a -> BitSet a -> Bool
member x (BitSet _n i) = testBit i (fromEnum x)
{-# INLINE member #-}

-- | /O(testBit on Integer)/ Ask whether the item is in the bit set.
notMember :: Enum a => a -> BitSet a -> Bool
notMember bs = not . member bs
{-# INLINE notMember #-}

-- | /O(max(n, m))/. Is this a subset? (@s1 isSubsetOf s2@) tells whether
-- @s1@ is a subset of @s2@.
isSubsetOf :: Enum a => BitSet a -> BitSet a -> Bool
isSubsetOf (BitSet n1 i1) (BitSet n2 i2) = n2 >= n1 && i2 .|. i1 == i2

-- | /O(max(n, m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: Enum a => BitSet a -> BitSet a -> Bool
isProperSubsetOf bs1 bs2 = bs1 `isSubsetOf` bs2 && bs1 /= bs2

-- | The empty bit set.
empty :: BitSet a
empty = BitSet 0 0
{-# INLINE empty #-}

-- | O(setBit on Integer). Create a singleton set.
singleton :: Enum a => a -> BitSet a
singleton x = insert x empty
{-# INLINE singleton #-}

-- | /O(setBit on Integer)/ Insert an item into the bit set.
insert :: Enum a => a -> BitSet a -> BitSet a
insert x (BitSet n i) = BitSet n' $ setBit i e where
  n' = if testBit i e then n else n + 1
  e  = fromEnum x
{-# INLINE insert #-}

-- | /O(clearBit on Integer)/ Delete an item from the bit set.
delete :: Enum a => a -> BitSet a -> BitSet a
delete x (BitSet n i) = BitSet n' $ clearBit i e where
  n' = if testBit i e then n - 1 else n
  e  = fromEnum x
{-# INLINE delete #-}

-- | /O(max(m, n))/ The union of two bit sets.
union :: Enum a => BitSet a -> BitSet a -> BitSet a
union (BitSet _n1 i1) (BitSet _n2 i2) = BitSet (popCount i) i where
  i = i1 .|. i2
{-# INLINE union #-}

-- | /O(max(m, n))/ The union of a list of bit sets.
unions :: Enum a => [BitSet a] -> BitSet a
unions = foldl' union empty
{-# INLINE unions #-}

-- | /O(max(m, n))/ Difference of two bit sets.
difference :: Enum a => BitSet a -> BitSet a -> BitSet a
difference (BitSet _n1 i1) (BitSet _n2 i2) = BitSet (popCount i) i where
  i = i1 .&. complement i2

(\\) :: Enum a => BitSet a -> BitSet a -> BitSet a
(\\) = difference

-- | /O(max(m, n))/ The intersection of two bit sets.
intersection :: Enum a => BitSet a -> BitSet a -> BitSet a
intersection (BitSet _n1 i1) (BitSet _n2 i2) = BitSet (popCount i) i where
  i = i1 .&. i2

-- | /O(n * shiftR on Integer)/ An alias to @toList@.
elems :: Enum a => BitSet a -> [a]
elems = toList

-- | /O(n * shiftR on Integer)/ Convert a bit set to a list of elements.
toList :: Enum a => BitSet a -> [a]
toList (BitSet _i n0) = go 0 n0 [] where
  go _i 0 acc = reverse acc
  go i n acc  = if n `testBit` 0
                then go (i + 1) (shiftR n 1) (toEnum i : acc)
                else go (i + 1) (shiftR n 1) acc

-- | /O(n * setBit on Integer)/ Make a bit set from a list of elements.
fromList :: Enum a => [a] -> BitSet a
fromList xs = BitSet (popCount i) i where
  i = foldl' (\b x -> setBit b (fromEnum x)) 0 xs

-- | /O(1)/ Project a bit set to an integral type.
toIntegral :: Integral b => BitSet a -> b
toIntegral (BitSet _n i) = fromIntegral i
{-# INLINE toIntegral #-}

-- | /O(n)/ Make a bit set from an integral. Unsafe because we don't
-- checked whether the bits set in a given value correspond to values
-- of type @a@. This is only useful as a more efficient alternative to
-- fromList.
unsafeFromIntegral :: Integral b => b -> BitSet a
unsafeFromIntegral x = let i = fromIntegral x in BitSet (popCount i) i
{-# INLINE unsafeFromIntegral #-}
