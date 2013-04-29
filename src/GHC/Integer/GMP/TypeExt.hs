{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Integer.GMP.TypeExt
    ( popCountInteger
    , testBitInteger
    , setBitInteger
    , clearBitInteger
    ) where

import GHC.Integer.GMP.Internals (Integer(..))
import GHC.Integer.GMP.Prim (int2Integer#)
import GHC.Prim (Int#, (/=#), word2Int#, int2Word#, popCnt#)

import GHC.Integer.GMP.PrimExt (popCountInteger#, testBitInteger#,
                                setBitInteger#, clearBitInteger#)

popCountInteger :: Integer -> Int#
popCountInteger (S# i)   = (word2Int# (popCnt# (int2Word# i)))
popCountInteger (J# s d) = popCountInteger# s d
{-# NOINLINE popCountInteger #-}

testBitInteger :: Integer -> Int# -> Bool
testBitInteger j@(S# _) i = testBitInteger (toBig j) i
testBitInteger (J# s d) i = testBitInteger# s d i /=# 0#
{-# NOINLINE testBitInteger #-}

setBitInteger :: Integer -> Int# -> Integer
setBitInteger j@(S# _) i = setBitInteger (toBig j) i
setBitInteger (J# s d) i =
    let (# s', d' #) = setBitInteger# s d i in J# s' d'
{-# NOINLINE setBitInteger #-}

clearBitInteger :: Integer -> Int# -> Integer
clearBitInteger j@(S# _) i = clearBitInteger (toBig j) i
clearBitInteger (J# s d) i =
    let (# s', d' #) = clearBitInteger# s d i in J# s' d'
{-# NOINLINE clearBitInteger #-}

toBig :: Integer -> Integer
toBig (S# i)     = case int2Integer# i of { (# s, d #) -> J# s d }
toBig i@(J# _ _) = i
