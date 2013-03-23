{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
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
                  testBit, setBit, clearBit, shiftR, popCount)
import Data.Data (Typeable)
import Data.Foldable (Foldable(foldMap), toList)
import Data.List (foldl')
import Data.Monoid (Monoid(..))
import Text.Read (Read(..), Lexeme(..), lexP, prec, parens)

import Control.DeepSeq (NFData(..))

data BitSet a = Enum a => BitSet { _n    :: {-# UNPACK #-} !Int
                                 , _bits :: !Integer
                                 }
    deriving Typeable

instance Eq (BitSet a) where
    BitSet { _n = n1, _bits = b1 } == BitSet { _n = n2 , _bits = b2 } =
        n1 == n2 && b1 == b2

instance Ord (BitSet a) where
    bs1 `compare` bs2 = case _n bs1 `compare` _n bs2 of
        EQ -> _bits bs1 `compare` _bits bs2
        r  -> r


instance (Enum a, Read a) => Read (BitSet a) where
    readPrec = parens . prec 10 $ do
        Ident "fromList" <- lexP
        fromList <$> readPrec

instance Show a => Show (BitSet a) where
    showsPrec p bs = showParen (p > 10) $
                     showString "fromList " . shows (toList bs)

instance Enum a => Monoid (BitSet a) where
    mempty  = empty
    mappend = union
    mconcat = unions

instance NFData (BitSet a) where
    rnf (BitSet { _n, _bits }) = rnf _n `seq` rnf _bits `seq` ()

instance Foldable BitSet where
    foldMap f (BitSet { _bits }) = go 0 _bits where
        go _b 0 = mempty
        go b i  =
            if i `testBit` 0
            then f (toEnum b) `mappend` go (b + 1) (shiftR i 1)
            else go (b + 1) (shiftR i 1)

-- | /O(1)/. Is the bit set empty?
null :: BitSet a -> Bool
null (BitSet { _bits }) = _bits == 0
{-# INLINE null #-}

-- | /O(1)/. The number of elements in the bit set.
size :: BitSet a -> Int
size = _n
{-# INLINE size #-}

-- | /O(testBit on Integer)/. Ask whether the item is in the bit set.
member :: a -> BitSet a -> Bool
member x (BitSet { _bits }) = testBit _bits (fromEnum x)
{-# INLINE member #-}

-- | /O(testBit on Integer)/. Ask whether the item is in the bit set.
notMember :: a -> BitSet a -> Bool
notMember bs = not . member bs
{-# INLINE notMember #-}

-- | /O(max(n, m))/. Is this a subset? (@s1 isSubsetOf s2@) tells whether
-- @s1@ is a subset of @s2@.
isSubsetOf :: BitSet a -> BitSet a -> Bool
isSubsetOf (BitSet { _n = n1, _bits = b1 }) (BitSet { _n = n2, _bits = b2 }) =
    n2 >= n1 && b2 .|. b1 == b2

-- | /O(max(n, m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: BitSet a -> BitSet a -> Bool
isProperSubsetOf bs1 bs2 = bs1 `isSubsetOf` bs2 && bs1 /= bs2

-- | The empty bit set.
empty :: Enum a => BitSet a
empty = BitSet { _n = 0, _bits = 0 }
{-# INLINE empty #-}

-- | O(setBit on Integer). Create a singleton set.
singleton :: Enum a => a -> BitSet a
singleton x = BitSet { _n = 1, _bits = bit $! fromEnum x }
{-# INLINE singleton #-}

-- | /O(setBit on Integer)/. Insert an item into the bit set.
insert :: a -> BitSet a -> BitSet a
insert x bs@(BitSet { _n, _bits }) =
    if testBit _bits i
    then bs
    else bs { _n = _n + 1, _bits = setBit _bits i }
  where
    i :: Int
    i = fromEnum x
{-# INLINE insert #-}

-- | /O(clearBit on Integer)/. Delete an item from the bit set.
delete :: a -> BitSet a -> BitSet a
delete x bs@(BitSet { _n, _bits }) =
    if testBit _bits i
    then bs { _n = _n - 1, _bits = clearBit _bits i }
    else bs
  where
    i :: Int
    i  = fromEnum x
{-# INLINE delete #-}

-- | /O(max(m, n))/. The union of two bit sets.
union :: BitSet a -> BitSet a -> BitSet a
union (BitSet { _bits = b1 }) (BitSet { _bits = b2 }) =
    BitSet { _n = popCount b, _bits = b }
  where
    b :: Integer
    b = b1 .|. b2
{-# INLINE union #-}

-- | /O(max(m, n))/. The union of a list of bit sets.
unions :: Enum a => [BitSet a] -> BitSet a
unions = foldl' union empty
{-# INLINE unions #-}

-- | /O(max(m, n))/. Difference of two bit sets.
difference :: BitSet a -> BitSet a -> BitSet a
difference (BitSet { _bits = b1 }) (BitSet { _bits = b2 }) =
    BitSet { _n = popCount b, _bits = b }
  where
    b :: Integer
    b = b1 .&. complement b2

-- | /O(max(m, n))/. See `difference'.
(\\) :: BitSet a -> BitSet a -> BitSet a
(\\) = difference

-- | /O(max(m, n))/. The intersection of two bit sets.
intersection :: BitSet a -> BitSet a -> BitSet a
intersection (BitSet { _bits = b1 }) (BitSet { _bits = b2 }) =
    BitSet { _n = popCount b, _bits = b }
  where
    b :: Integer
    b = b1 .&. b2

-- | /O(n * shiftR on Integer)/. An alias to @toList@.
elems :: BitSet a -> [a]
elems = toList

-- | /O(n * setBit on Integer)/. Make a bit set from a list of elements.
fromList :: Enum a => [a] -> BitSet a
fromList xs = BitSet { _n = popCount b, _bits = b } where
  b = foldl' (\i x -> setBit i (fromEnum x)) 0 xs
