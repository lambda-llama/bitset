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

    -- * Folds
    , foldl'
    , foldr

    -- * Filter
    , filter

    -- * Lists
    , toList
    , fromList
    ) where

import Prelude hiding (null, map, filter, foldr)

import Data.Word (Word)

import Data.BitSet.Generic (GBitSet)
import qualified Data.BitSet.Generic as GS

type BitSet = GBitSet Word

-- | /O(1)/. Is the bit set empty?
null :: BitSet a -> Bool
null = GS.null
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the bit set.
size :: BitSet a -> Int
size = GS.size
{-# INLINE size #-}

-- | /O(1)/. Ask whether the item is in the bit set.
member :: Enum a => a -> BitSet a -> Bool
member = GS.member
{-# INLINE member #-}

-- | /O(1)/. Ask whether the item is in the bit set.
notMember :: Enum a => a -> BitSet a -> Bool
notMember = GS.notMember
{-# INLINE notMember #-}

-- | /O(max(n, m))/. Is this a subset? (@s1 isSubsetOf s2@) tells whether
-- @s1@ is a subset of @s2@.
isSubsetOf :: BitSet a -> BitSet a -> Bool
isSubsetOf = GS.isSubsetOf

-- | /O(max(n, m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: BitSet a -> BitSet a -> Bool
isProperSubsetOf = GS.isProperSubsetOf

-- | The empty bit set.
empty :: Enum a => BitSet a
empty = GS.empty
{-# INLINE empty #-}

-- | O(1). Create a singleton set.
singleton :: Enum a => a -> BitSet a
singleton = GS.singleton
{-# INLINE singleton #-}

-- | /O(1)/. Insert an item into the bit set.
insert :: a -> BitSet a -> BitSet a
insert = GS.insert
{-# INLINE insert #-}

-- | /O(1)/. Delete an item from the bit set.
delete :: a -> BitSet a -> BitSet a
delete = GS.delete
{-# INLINE delete #-}

-- | /O(max(m, n))/. The union of two bit sets.
union :: BitSet a -> BitSet a -> BitSet a
union = GS.union
{-# INLINE union #-}

-- | /O(max(m, n))/. The union of a list of bit sets.
unions :: Enum a => [BitSet a] -> BitSet a
unions = GS.unions
{-# INLINE unions #-}

-- | /O(max(m, n))/. Difference of two bit sets.
difference :: BitSet a -> BitSet a -> BitSet a
difference = GS.difference
{-# INLINE difference #-}

-- | /O(max(m, n))/. See `difference'.
(\\) :: BitSet a -> BitSet a -> BitSet a
(\\) = difference

-- | /O(max(m, n))/. The intersection of two bit sets.
intersection :: BitSet a -> BitSet a -> BitSet a
intersection = GS.intersection
{-# INLINE intersection #-}

-- | /O(n)/ Transform this bit set by applying a function to every value.
-- Resulting bit set may be smaller then the original.
map :: (Enum a, Enum b) => (a -> b) -> BitSet a -> BitSet b
map = GS.map

-- | /O(n)/ Reduce this bit set by applying a binary function to all
-- elements, using the given starting value.  Each application of the
-- operator is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldl' :: (b -> a -> b) -> b -> BitSet a -> b
foldl' = GS.foldl'
{-# INLINE foldl' #-}

-- | /O(n)/ Reduce this bit set by applying a binary function to all
-- elements, using the given starting value.
foldr :: (a -> b -> b) -> b -> BitSet a -> b
foldr = GS.foldr
{-# INLINE foldr #-}

-- | /O(n)/ Filter this bit set by retaining only elements satisfying a
-- predicate.
filter :: Enum a => (a -> Bool) -> BitSet a -> BitSet a
filter = GS.filter

-- | /O(n)/. Convert the bit set set to a list of elements.
toList :: BitSet a -> [a]
toList = GS.toList

-- | /O(n)/. Make a bit set from a list of elements.
fromList :: Enum a => [a] -> BitSet a
fromList = GS.fromList
{-# INLINE fromList #-}
