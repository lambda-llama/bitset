{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.BitSet.Types
    ( GBitSet(..)
    ) where

import Data.Bits (Bits)
import Data.Data (Typeable)


data GBitSet c a = (Enum a, Bits c, Num c) =>
                   BitSet { _n    :: Int
                          , _bits :: c
                          }
    deriving Typeable
