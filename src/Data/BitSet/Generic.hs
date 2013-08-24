-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitSet.Generic
-- Copyright   :  (c) Sergei Lebedev, Aleksey Kladov, Fedor Gogolev 2013
--                Based on Data.BitSet (c) Denis Bueno 2008-2009
-- License     :  MIT
-- Maintainer  :  superbobry@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- A space-efficient implementation of set data structure for enumerated
-- data types.
--
-- /Note/: Read below the synopsis for important notes on the use of
-- this module.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import Data.BitSet.Generic (BitSet)
-- > import qualified Data.BitSet.Generic as BS
--
-- The implementation is abstract with respect to container type, so any
-- numeric type with 'Bits' instance can be used as a container. However,
-- independent of container choice, the maximum number of elements in a
-- bit set is bounded by @maxBound :: Int@.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BitSet.Generic
    (
    -- * Bit set type
      BitSet(..)

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

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Data.Bits (Bits, (.|.), (.&.), complement, bit,
                  testBit, setBit, clearBit, popCount)
import Data.Data (Typeable)
import Data.Monoid (Monoid(..))
import Foreign (Storable)
import GHC.Exts (build)
import Text.Read (Read(..), Lexeme(..), lexP, prec, parens)
import qualified Data.List as List

-- | A bit set with unspecified container type.
newtype BitSet c a = BitSet { getBits :: c }
   deriving (Eq, NFData, Storable, Ord, Typeable)

instance (Enum a, Read a, Bits c, Num c) => Read (BitSet c a) where
    readPrec = parens . prec 10 $ do
        Ident "fromList" <- lexP
        fromList <$> readPrec

instance (Enum a, Show a, Bits c,  Num c) => Show (BitSet c a) where
    showsPrec p bs = showParen (p > 10) $
                     showString "fromList " . shows (toList bs)

instance (Enum a, Bits c, Num c) => Monoid (BitSet c a) where
    mempty  = empty
    mappend = union

-- | /O(1)/. Is the bit set empty?
null :: (Eq c, Num c) => BitSet c a -> Bool
null = (== 0) . getBits
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the bit set.
size :: Bits c => BitSet c a -> Int
size = popCount . getBits
{-# INLINE size #-}

-- | /O(d)/. Ask whether the item is in the bit set.
member :: (Enum a , Bits c) => a -> BitSet c a -> Bool
member x = (`testBit` fromEnum x) . getBits
{-# INLINE member #-}

-- | /O(d)/. Ask whether the item is not in the bit set.
notMember :: (Enum a, Bits c) => a -> BitSet c a -> Bool
notMember x = not . member x
{-# INLINE notMember #-}

-- | /O(max(n, m))/. Is this a subset? (@s1 `isSubsetOf` s2@) tells whether
-- @s1@ is a subset of @s2@.
isSubsetOf :: (Bits c, Eq c) => BitSet c a -> BitSet c a -> Bool
isSubsetOf (BitSet bits1) (BitSet bits2) = bits2 .|. bits1 == bits2
{-# INLINE isSubsetOf #-}

-- | /O(max(n, m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: (Bits c, Eq c) => BitSet c a -> BitSet c a -> Bool
isProperSubsetOf bs1 bs2 = bs1 `isSubsetOf` bs2 && bs1 /= bs2
{-# INLINE isProperSubsetOf #-}

-- | The empty bit set.
empty :: (Enum a, Bits c, Num c) => BitSet c a
empty = BitSet 0
{-# INLINE empty #-}

-- | O(1). Create a singleton set.
singleton :: (Enum a, Bits c, Num c) => a -> BitSet c a
singleton = BitSet . bit . fromEnum
{-# INLINE singleton #-}

-- | /O(d)/. Insert an item into the bit set.
insert :: (Enum a, Bits c) => a -> BitSet c a -> BitSet c a
insert x (BitSet bits) = BitSet $ bits `setBit` fromEnum x
{-# INLINE insert #-}

-- | /O(d)/. Delete an item from the bit set.
delete :: (Enum a, Bits c) => a -> BitSet c a -> BitSet c a
delete x (BitSet bits ) = BitSet $ bits `clearBit` fromEnum x
{-# INLINE delete #-}

-- | /O(max(m, n))/. The union of two bit sets.
union :: Bits c => BitSet c a -> BitSet c a -> BitSet c a
union (BitSet bits1) (BitSet bits2) = BitSet $ bits1 .|. bits2
{-# INLINE union #-}

-- | /O(max(m, n))/. Difference of two bit sets.
difference :: Bits c => BitSet c a -> BitSet c a -> BitSet c a
difference (BitSet bits1) (BitSet bits2) = BitSet $ bits1 .&. complement bits2
{-# INLINE difference #-}

-- | /O(max(m, n))/. See 'difference'.
(\\) :: Bits c => BitSet c a -> BitSet c a -> BitSet c a
(\\) = difference

-- | /O(max(m, n))/. The intersection of two bit sets.
intersection :: Bits c => BitSet c a -> BitSet c a -> BitSet c a
intersection (BitSet bits1) (BitSet bits2) = BitSet $ bits1 .&. bits2
{-# INLINE intersection #-}

-- | /O(d * n)/ Transform this bit set by applying a function to every
-- value.  Resulting bit set may be smaller then the original.
map :: (Enum a, Enum b, Bits c, Num c) => (a -> b) -> BitSet c a -> BitSet c b
map f = foldl' (\bs -> (`insert` bs) . f) empty
{-# INLINE map #-}

-- | /O(d * n)/ Reduce this bit set by applying a binary function to all
-- elements, using the given starting value.  Each application of the
-- operator is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldl' :: (Enum a, Bits c) => (b -> a -> b) -> b -> BitSet c a -> b
foldl' f acc0  (BitSet bits) = go acc0 (popCount bits) 0 where
  go !acc 0 _b = acc
  go !acc !n b = if bits `testBit` b
                 then go (f acc $ toEnum b) (pred n) (succ b)
                 else go acc n (succ b)
{-# INLINE foldl' #-}

-- | /O(d * n)/ Reduce this bit set by applying a binary function to
-- all elements, using the given starting value.
foldr :: (Enum a, Bits c) => (a -> b -> b) -> b -> BitSet c a -> b
foldr f acc0 (BitSet bits) = go (popCount bits) 0 where
  go 0 _b = acc0
  go !n b = if bits `testBit` b
            then toEnum b `f` go (pred n) (succ b)
            else go n (succ b)
{-# INLINE foldr #-}

-- | /O(d * n)/ Filter this bit set by retaining only elements satisfying
-- predicate.
filter :: (Enum a, Bits c, Num c) => (a -> Bool) -> BitSet c a -> BitSet c a
filter f = foldl' (\bs x -> if f x then x `insert` bs else bs) empty
{-# INLINE filter #-}

-- | /O(d * n)/. Convert this bit set set to a list of elements.
toList :: (Enum a, Bits c, Num c) => BitSet c a -> [a]
toList bs = build (\k z -> foldr k z bs)
{-# INLINE [0] toList #-}

-- | /O(d * n)/. Make a bit set from a list of elements.
fromList :: (Enum a, Bits c, Num c) => [a] -> BitSet c a
fromList = BitSet . List.foldl' (\i x -> i `setBit` fromEnum x) 0
{-# INLINE [0] fromList #-}
{-# RULES
"fromList/toList"    forall bs. fromList (toList bs) = bs
  #-}
