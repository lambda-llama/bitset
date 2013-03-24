{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitSet
-- Copyright   :  (c) Sergei Lebedev, Aleksey Kladov 2013
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
--
-----------------------------------------------------------------------------

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
    ) where

import Prelude hiding (null)

import Control.Applicative ((<$>))
import Data.Bits (Bits, (.|.), (.&.), complement, bit,
                  testBit, setBit, clearBit, popCount)
import Data.Function (on)
import Data.List (foldl')
import Data.Monoid (Monoid(..), (<>))
import Text.Read (Read(..), Lexeme(..), lexP, prec, parens)
import qualified Data.Foldable as Foldable

import Control.DeepSeq (NFData(..))

import Data.BitSet.Types (GBitSet(..))

type BitSet = GBitSet

instance Eq (BitSet c a) where
    (==) = (==) `on` _n

instance Ord (BitSet c a) where
    compare = compare `on` _n

instance (Enum a, Read a, Bits c, Num c) => Read (BitSet c a) where
    readPrec = parens . prec 10 $ do
        Ident "fromList" <- lexP
        fromList <$> readPrec

instance (Show a, Num c) => Show (BitSet c a) where
    showsPrec p bs = showParen (p > 10) $
                     showString "fromList " . shows (toList bs)

instance (Enum a, Bits c, Num c) => Monoid (BitSet c a) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance NFData c => NFData (BitSet c a) where
    rnf (BitSet { _n, _bits }) = rnf _n `seq` rnf _bits `seq` ()

instance Num c => Foldable.Foldable (BitSet c) where
    foldMap f (BitSet { _n, _bits }) = go _n 0 where
        go 0 _b = mempty
        go !n b = if _bits `testBit` b
                  then f (toEnum b) <> go (pred n) (succ b)
                  else go n (succ b)

-- | /O(1)/. Is the bit set empty?
null :: BitSet c a -> Bool
null (BitSet { _bits }) = _bits == 0
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the bit set.
size :: BitSet c a -> Int
size = _n
{-# INLINE size #-}

-- | /O(1)/. Ask whether the item is in the bit set.
member :: a -> BitSet c a -> Bool
member x (BitSet { _bits }) = _bits `testBit` fromEnum x
{-# INLINE member #-}

-- | /O(1)/. Ask whether the item is in the bit set.
notMember :: a -> BitSet c a -> Bool
notMember x = not . member x
{-# INLINE notMember #-}

-- | /O(max(n, m))/. Is this a subset? (@s1 isSubsetOf s2@) tells whether
-- @s1@ is a subset of @s2@.
isSubsetOf :: BitSet c a -> BitSet c a -> Bool
isSubsetOf (BitSet { _n = n1, _bits = b1 }) (BitSet { _n = n2, _bits = b2 }) =
    n2 >= n1 && b2 .|. b1 == b2

-- | /O(max(n, m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: Eq c => BitSet c a -> BitSet c a -> Bool
isProperSubsetOf bs1 bs2 = bs1 `isSubsetOf` bs2 && bs1 /= bs2

-- | The empty bit set.
empty :: (Enum a, Bits c, Num c) => BitSet c a
empty = BitSet { _n = 0, _bits = 0 }
{-# INLINE empty #-}

-- | O(setBit on Integer). Create a singleton set.
singleton :: (Enum a, Bits c, Num c) => a -> BitSet c a
singleton x = BitSet { _n = 1, _bits = bit $! fromEnum x }
{-# INLINE singleton #-}

-- | /O(d)/. Insert an item into the bit set.
insert :: a -> BitSet c a -> BitSet c a
insert x bs@(BitSet { _bits }) =
    let b = _bits `setBit` fromEnum x in bs { _n = popCount b, _bits = b }
{-# INLINE insert #-}

-- | /O(d)/. Delete an item from the bit set.
delete :: a -> BitSet c a -> BitSet c a
delete x bs@(BitSet { _bits }) =
    let b = _bits `clearBit` fromEnum x in bs { _n = popCount b, _bits = b }
{-# INLINE delete #-}

-- | /O(max(m, n))/. The union of two bit sets.
union :: BitSet c a -> BitSet c a -> BitSet c a
union (BitSet { _bits = b1 }) (BitSet { _bits = b2 }) =
    let b = b1 .|. b2 in BitSet { _n = popCount b, _bits = b }

{-# INLINE union #-}

-- | /O(max(m, n))/. The union of a list of bit sets.
unions :: (Enum a, Bits c, Num c) => [BitSet c a] -> BitSet c a
unions = foldl' union empty
{-# INLINE unions #-}

-- | /O(max(m, n))/. Difference of two bit sets.
difference :: BitSet c a -> BitSet c a -> BitSet c a
difference (BitSet { _bits = b1 }) (BitSet { _bits = b2 }) =
    let b = b1 .&. complement b2 in BitSet { _n = popCount b, _bits = b }
{-# INLINE difference #-}

-- | /O(max(m, n))/. See 'difference'.
(\\) :: BitSet c a -> BitSet c a -> BitSet c a
(\\) = difference

-- | /O(max(m, n))/. The intersection of two bit sets.
intersection :: BitSet c a -> BitSet c a -> BitSet c a
intersection (BitSet { _bits = b1 }) (BitSet { _bits = b2 }) =
    BitSet { _n = popCount b, _bits = b }
  where
    b = b1 .&. b2
{-# INLINE intersection #-}

-- | /O(d * n)/. Convert the bit set set to a list of elements.
toList :: Num c => BitSet c a -> [a]
toList = Foldable.toList

-- | /O(d * n)/. An alias to 'toList'.
elems :: Num c => BitSet c a -> [a]
elems = toList
{-# INLINE elems #-}

-- | /O(d * n)/. Make a bit set from a list of elements.
fromList :: (Enum a, Bits c, Num c) => [a] -> BitSet c a
fromList xs = BitSet { _n = popCount b, _bits = b } where
  b = foldl' (\i x -> setBit i (fromEnum x)) 0 xs
{-# INLINE fromList #-}
