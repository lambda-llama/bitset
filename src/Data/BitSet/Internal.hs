{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.BitSet.Internal
    ( GBitSet(..)
    ) where

import Data.Bits (Bits)
import Data.Data (Typeable)


-- | A bit set with unspecified container type.
data GBitSet c a = (Enum a, Bits c, Num c) =>
                   BitSet { _n    :: Int  -- ^ Number of elements in the bit set.
                          , _bits :: c    -- ^ Bit container.
                          }
    deriving Typeable
