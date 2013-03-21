{-# LANGUAGE ScopedTypeVariables #-}
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
    ( BitSet
    , empty
    , singleton
    , insert
    , delete
    , fromList
    , notMember
    , member
    , null
    , size
    , toIntegral
    , unsafeFromIntegral
    ) where

import Prelude hiding (null)

import Data.Bits (testBit, setBit, clearBit, shiftR, popCount)
import Data.Data (Data, Typeable)
import Data.List (foldl')

data BitSet a = BitSet {-# UNPACK #-} !Int Integer
    deriving (Eq, Ord, Data, Typeable)

instance (Enum a, Show a) => Show (BitSet a) where
    show (BitSet _ i :: BitSet a) = "fromList " ++ show (f 0 i) where
      f _n 0 = []
      f n x  = if testBit x 0
               then (toEnum n :: a) : f (n + 1) (shiftR x 1)
               else f (n + 1) (shiftR x 1)

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
insert x (BitSet count i) = BitSet count' (setBit i e)
    where count' = if testBit i e then count else count+1
          e      = fromEnum x
{-# INLINE insert #-}

-- | /O(clearBit on Integer)/ Delete an item from the bit set.
delete :: Enum a => a -> BitSet a -> BitSet a
delete x (BitSet count i) = BitSet count' (clearBit i e)
    where count' = if testBit i e then count - 1 else count
          e      = fromEnum x
{-# INLINE delete #-}

-- | /O(testBit on Integer)/ Ask whether the item is in the bit set.
member :: Enum a => a -> BitSet a -> Bool
member x (BitSet _ i) = testBit i (fromEnum x)
{-# INLINE member #-}

-- | /O(testBit on Integer)/ Ask whether the item is in the bit set.
notMember :: Enum a => a -> BitSet a -> Bool
notMember bs = not . member bs
{-# INLINE notMember #-}

-- | /O(1)/ The number of elements in the bit set.
size :: BitSet a -> Int
size (BitSet count _) = count
{-# INLINE size #-}

-- | /O(n * setBit on Integer)/ Make a @BitSet@ from a list of items.
fromList :: Enum a => [a] -> BitSet a
fromList xs = BitSet (length xs) (foldl' (\i x -> setBit i (fromEnum x)) 0 xs)

-- | Is the bit set empty?
null :: BitSet a -> Bool
null (BitSet n _) = n == 0
{-# INLINE null #-}

-- | /O(1)/ Project a bit set to an integer.
toIntegral :: Integral b => BitSet a -> b
toIntegral (BitSet _ i) = fromIntegral i
{-# INLINE toIntegral #-}

-- | /O(n)/ Make a bit set of type @BitSet a@ from an integer. This is unsafe
-- because it is not checked whether the bits set in the integer correspond to
-- values of type @a@. This is only useful as a more efficient alternative to
-- fromList.
unsafeFromIntegral :: Integral b => b -> BitSet a
unsafeFromIntegral x = let i = fromIntegral x in BitSet (popCount i) i
{-# INLINE unsafeFromIntegral #-}
