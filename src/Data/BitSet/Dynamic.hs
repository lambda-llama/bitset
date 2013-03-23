module Data.BitSet.Dynamic
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
    ) where

import Prelude hiding (null)

import qualified Data.BitSet as BS

type BitSet a = BS.BitSet Integer a

-- | /O(1)/. Is the bit set empty?
null :: BitSet a -> Bool
null = BS.null
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the bit set.
size :: BitSet a -> Int
size = BS.size
{-# INLINE size #-}

-- | /O(1)/. Ask whether the item is in the bit set.
member :: a -> BitSet a -> Bool
member = BS.member
{-# INLINE member #-}

-- | /O(1)/. Ask whether the item is in the bit set.
notMember :: a -> BitSet a -> Bool
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

-- | /O(n)/. Convert the bit set set to a list of elements.
toList :: BitSet a -> [a]
toList = BS.toList

-- | /O(n)/. An alias to @toList@.
elems :: BitSet a -> [a]
elems = BS.toList
{-# INLINE elems #-}

-- | /O(n)/. Make a bit set from a list of elements.
fromList :: Enum a => [a] -> BitSet a
fromList = BS.fromList
{-# INLINE fromList #-}
