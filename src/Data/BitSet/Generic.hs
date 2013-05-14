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

{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.BitSet.Generic
    (
    -- * Bit set type
      GBitSet

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

    -- * Internal
    , toBits
    , unsafeFromBits
    ) where

import Prelude hiding (null, map, filter, foldr)

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Data.Bits (Bits, (.|.), (.&.), complement, bit,
                  testBit, setBit, clearBit, popCount)
import Data.Data (Typeable)
import Data.Monoid (Monoid(..), (<>))
import Foreign (Storable(..), castPtr)
import GHC.Exts (build)
import Text.Read (Read(..), Lexeme(..), lexP, prec, parens)
import qualified Data.Foldable as Foldable
import qualified Data.List as List

-- | A bit set with unspecified container type.
data GBitSet c a =
    (Enum a, Bits c, Num c) =>
    BitSet { _n    :: {-# UNPACK #-} !Int  -- ^ Number of elements in the bit set.
           , _bits :: !c                   -- ^ Bit container.
           }
    deriving Typeable

instance Eq c => Eq (GBitSet c a) where
    BitSet { _n = n1, _bits = b1 } == BitSet { _n = n2, _bits = b2 } =
        n1 == n2 && b1 == b2

instance Ord c => Ord (GBitSet c a) where
    BitSet { _n = n1, _bits = b1 } `compare` BitSet { _n = n2, _bits = b2 } =
        case compare n1 n2 of
            EQ  -> compare b1 b2
            res -> res

instance (Enum a, Read a, Bits c, Num c) => Read (GBitSet c a) where
    readPrec = parens . prec 10 $ do
        Ident "fromList" <- lexP
        fromList <$> readPrec

instance (Show a, Num c) => Show (GBitSet c a) where
    showsPrec p bs = showParen (p > 10) $
                     showString "fromList " . shows (toList bs)

instance (Enum a, Bits c, Num c) => Monoid (GBitSet c a) where
    mempty  = empty
    mappend = union

instance NFData c => NFData (GBitSet c a) where
    rnf (BitSet { _n, _bits }) = rnf _n `seq` rnf _bits `seq` ()

instance (Bits c, Enum a, Num c, Storable c) => Storable (GBitSet c a) where
    sizeOf = sizeOf . _bits
    alignment = alignment . _bits
    peek ptr = do
        b <- peek $ castPtr ptr
        return $! BitSet (popCount b) b
    poke ptr = poke (castPtr ptr) . _bits

instance Num c => Foldable.Foldable (GBitSet c) where
#if MIN_VERSION_base(4, 6, 0)
    foldl' = foldl'
#endif
    foldr  = foldr

    foldMap f (BitSet { _n, _bits }) = go _n 0 where
        go 0 _b = mempty
        go !n b = if _bits `testBit` b
                  then f (toEnum b) <> go (pred n) (succ b)
                  else go n (succ b)

-- | /O(1)/. Is the bit set empty?
null :: GBitSet c a -> Bool
null (BitSet { _n = 0, _bits = 0 }) = True
null _bs = False
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the bit set.
size :: GBitSet c a -> Int
size = _n
{-# INLINE size #-}

-- | /O(d)/. Ask whether the item is in the bit set.
member :: (Enum a , Bits c) => a -> GBitSet c a -> Bool
member x = (`testBit` fromEnum x) . _bits
{-# INLINE member #-}

-- | /O(d)/. Ask whether the item is in the bit set.
notMember :: (Enum a, Bits c) => a -> GBitSet c a -> Bool
notMember x = not . member x
{-# INLINE notMember #-}

-- | /O(max(n, m))/. Is this a subset? (@s1 isSubsetOf s2@) tells whether
-- @s1@ is a subset of @s2@.
isSubsetOf :: GBitSet c a -> GBitSet c a -> Bool
isSubsetOf (BitSet { _n = n1, _bits = b1 }) (BitSet { _n = n2, _bits = b2 }) =
    n2 >= n1 && b2 .|. b1 == b2
{-# INLINE isSubsetOf #-}

-- | /O(max(n, m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: Eq c => GBitSet c a -> GBitSet c a -> Bool
isProperSubsetOf bs1 bs2 = bs1 `isSubsetOf` bs2 && bs1 /= bs2
{-# INLINE isProperSubsetOf #-}

-- | The empty bit set.
empty :: (Enum a, Bits c, Num c) => GBitSet c a
empty = BitSet { _n = 0, _bits = 0 }
{-# INLINE empty #-}

-- | O(1). Create a singleton set.
singleton :: (Enum a, Bits c, Num c) => a -> GBitSet c a
singleton x = BitSet { _n = 1, _bits = bit $! fromEnum x }
{-# INLINE singleton #-}

-- | /O(d)/. Insert an item into the bit set.
insert :: a -> GBitSet c a -> GBitSet c a
insert x bs@(BitSet { _bits }) =
    let b = _bits `setBit` fromEnum x in bs { _n = popCount b, _bits = b }
{-# INLINE insert #-}

-- | /O(d)/. Delete an item from the bit set.
delete :: a -> GBitSet c a -> GBitSet c a
delete x bs@(BitSet { _bits }) =
    let b = _bits `clearBit` fromEnum x in bs { _n = popCount b, _bits = b }
{-# INLINE delete #-}

-- | /O(max(m, n))/. The union of two bit sets.
union :: GBitSet c a -> GBitSet c a -> GBitSet c a
union (BitSet { _bits = b1 }) (BitSet { _bits = b2 }) =
    let b = b1 .|. b2 in BitSet { _n = popCount b, _bits = b }
{-# INLINE union #-}

-- | /O(max(m, n))/. Difference of two bit sets.
difference :: GBitSet c a -> GBitSet c a -> GBitSet c a
difference (BitSet { _bits = b1 }) (BitSet { _bits = b2 }) =
    let b = b1 .&. complement b2 in BitSet { _n = popCount b, _bits = b }
{-# INLINE difference #-}

-- | /O(max(m, n))/. See 'difference'.
(\\) :: GBitSet c a -> GBitSet c a -> GBitSet c a
(\\) = difference

-- | /O(max(m, n))/. The intersection of two bit sets.
intersection :: GBitSet c a -> GBitSet c a -> GBitSet c a
intersection (BitSet { _bits = b1 }) (BitSet { _bits = b2 }) =
    BitSet { _n = popCount b, _bits = b }
  where
    b = b1 .&. b2
{-# INLINE intersection #-}

-- | /O(d * n)/ Transform this bit set by applying a function to every
-- value.  Resulting bit set may be smaller then the original.
map :: (Enum a, Enum b, Bits c, Num c) => (a -> b) -> GBitSet c a -> GBitSet c b
map f = fromList . List.map f . toList
{-# INLINE map #-}

-- | /O(d * n)/ Reduce this bit set by applying a binary function to all
-- elements, using the given starting value.  Each application of the
-- operator is evaluated before before using the result in the next
-- application.  This function is strict in the starting value.
foldl' :: (b -> a -> b) -> b -> GBitSet c a -> b
foldl' f acc0  (BitSet { _n, _bits }) = go acc0 _n 0 where
  go !acc 0 _b = acc
  go !acc !n b = if _bits `testBit` b
                 then go (f acc $ toEnum b) (pred n) (succ b)
                 else go acc n (succ b)
{-# INLINE foldl' #-}

-- | /O(d * n)/ Reduce this bit set by applying a binary function to
-- all elements, using the given starting value.
foldr :: (a -> b -> b) -> b -> GBitSet c a -> b
foldr f acc0 (BitSet { _n, _bits }) = go _n 0 where
  go 0 _b = acc0
  go !n b = if _bits `testBit` b
            then toEnum b `f` go (pred n) (succ b)
            else go n (succ b)
{-# INLINE foldr #-}

-- | /O(d * n)/ Filter this bit set by retaining only elements satisfying
-- predicate.
filter :: (Enum a, Bits c, Num c) => (a -> Bool) -> GBitSet c a -> GBitSet c a
filter f = fromList . List.filter f . toList
{-# INLINE filter #-}

-- | /O(d * n)/. Convert this bit set set to a list of elements.
toList :: Num c => GBitSet c a -> [a]
toList bs = build (\k z -> foldr k z bs)
{-# INLINE toList #-}

-- | /O(d * n)/. Make a bit set from a list of elements.
fromList :: (Enum a, Bits c, Num c) => [a] -> GBitSet c a
fromList xs = BitSet { _n = popCount b, _bits = b } where
  b = List.foldl' (\i x -> i `setBit` fromEnum x) 0 xs
{-# INLINE fromList #-}

-- | /O(1)/. Internal function, which extracts the underlying container
-- from the bit set.
toBits :: GBitSet c a -> c
toBits = _bits
{-# INLINE toBits #-}

-- | /O(1)/. Internal function, which constructs a bit set, using a given
-- container value. Highly unsafe, because we don't check if bits in the
-- given value correspond to valid instances of type @a@.
unsafeFromBits :: (Enum a, Bits c, Num c) => c -> GBitSet c a
unsafeFromBits b = BitSet { _n = popCount b, _bits = b }
{-# INLINE unsafeFromBits #-}
