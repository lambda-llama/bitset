{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module GHC.Integer.GMP.TypeExt
    ( popCountInteger
    , testBitInteger
    , setBitInteger
    , clearBitInteger
    ) where

import Data.Bits (popCount, testBit, clearBit)
import GHC.Base (Int(..))
import GHC.Integer.GMP.Internals (Integer(..))
import GHC.Integer.GMP.Prim (int2Integer#)
import GHC.Prim (Int#, (/=#))

import GHC.Integer.GMP.PrimExt (popCountInteger#, testBitInteger#,
                                setBitInteger#, clearBitInteger#)

popCountInteger :: Integer -> Int#
popCountInteger (S# i)   = let !(I# n) = popCount (I# i) in n
popCountInteger (J# s d) = popCountInteger# s d
{-# NOINLINE popCountInteger #-}

testBitInteger :: Integer -> Int# -> Bool
testBitInteger (S# j) i   = testBit (I# j) (I# i)
testBitInteger (J# s d) i = testBitInteger# s d i /=# 0#
{-# NOINLINE testBitInteger #-}

setBitInteger :: Integer -> Int# -> Integer
setBitInteger (S# j) i   =
    let !(# s, d #) = int2Integer# j in setBitInteger (J# s d) i
setBitInteger (J# s d) i =
    let !(# s', d' #) = setBitInteger# s d i in J# s' d'
{-# NOINLINE setBitInteger #-}

clearBitInteger :: Integer -> Int# -> Integer
clearBitInteger (S# j) i   = let !(I# j') = clearBit (I# j) (I# i) in S# j'
clearBitInteger (J# s d) i =
    let !(# s', d' #) = clearBitInteger# s d i in J# s' d'
{-# NOINLINE clearBitInteger #-}
