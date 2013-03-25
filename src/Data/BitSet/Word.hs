module Data.BitSet.Word
    (
    -- * Bit set type
      BitSet
    -- * Operators
    , (\\)

    -- * Construction
    , empty
    , singleton
    , insert
    , delete

    -- * Query
    , null
    , size
    , member
    , notMember
    , isSubsetOf
    , isProperSubsetOf

    -- * Combine
    , union
    , unions
    , difference
    , intersection

    -- * Transformations
    , map

    -- * Filter
    , filter

    -- * Lists
    , toList
    , fromList
    ) where

import Prelude hiding (null, map, filter)

import Data.Word (Word)

import Data.BitSet.Generic (GBitSet)
import qualified Data.BitSet.Generic as BS

type BitSet = GBitSet Word

-- | /O(1)/. Is the bit set empty?
null :: BitSet a -> Bool
null = BS.null
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the bit set.
size :: BitSet a -> Int
size = BS.size
{-# INLINE size #-}

-- | /O(1)/. Ask whether the item is in the bit set.
member :: Enum a => a -> BitSet a -> Bool
member = BS.member
{-# INLINE member #-}

-- | /O(1)/. Ask whether the item is in the bit set.
notMember :: Enum a => a -> BitSet a -> Bool
notMember = BS.notMember
{-# INLINE notMember #-}

-- | /O(max(n, m))/. Is this a subset? (@s1 isSubsetOf s2@) tells whether
-- @s1@ is a subset of @s2@.
isSubsetOf :: BitSet a -> BitSet a -> Bool
isSubsetOf = BS.isSubsetOf

-- | /O(max(n, m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: BitSet a -> BitSet a -> Bool
isProperSubsetOf = BS.isProperSubsetOf

-- | The empty bit set.
empty :: Enum a => BitSet a
empty = BS.empty
{-# INLINE empty #-}

-- | O(1). Create a singleton set.
singleton :: Enum a => a -> BitSet a
singleton = BS.singleton
{-# INLINE singleton #-}

-- | /O(1)/. Insert an item into the bit set.
insert :: a -> BitSet a -> BitSet a
insert = BS.insert
{-# INLINE insert #-}

-- | /O(1)/. Delete an item from the bit set.
delete :: a -> BitSet a -> BitSet a
delete = BS.delete
{-# INLINE delete #-}

-- | /O(max(m, n))/. The union of two bit sets.
union :: BitSet a -> BitSet a -> BitSet a
union = BS.union
{-# INLINE union #-}

-- | /O(max(m, n))/. The union of a list of bit sets.
unions :: Enum a => [BitSet a] -> BitSet a
unions = BS.unions
{-# INLINE unions #-}

-- | /O(max(m, n))/. Difference of two bit sets.
difference :: BitSet a -> BitSet a -> BitSet a
difference = BS.difference
{-# INLINE difference #-}

-- | /O(max(m, n))/. See `difference'.
(\\) :: BitSet a -> BitSet a -> BitSet a
(\\) = difference

-- | /O(max(m, n))/. The intersection of two bit sets.
intersection :: BitSet a -> BitSet a -> BitSet a
intersection = BS.intersection
{-# INLINE intersection #-}

-- | /O(n)/ Transform this bit set by applying a function to every value.
-- Resulting bit set may be smaller then the original.
map :: (Enum a, Enum b) => (a -> b) -> BitSet a -> BitSet b
map = BS.map

-- | /O(n)/ Filter this bit set by retaining only elements satisfying a
-- predicate.
filter :: Enum a => (a -> Bool) -> BitSet a -> BitSet a
filter = BS.filter

-- | /O(n)/. Convert the bit set set to a list of elements.
toList :: BitSet a -> [a]
toList = BS.toList

-- | /O(n)/. Make a bit set from a list of elements.
fromList :: Enum a => [a] -> BitSet a
fromList = BS.fromList
{-# INLINE fromList #-}
