{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}

module GHC.Integer.GMP.TypeExt
    ( popCountInteger
    , testBitInteger
    , setBitInteger
    , clearBitInteger
    ) where

#include "MachDeps.h"

import GHC.Integer.GMP.Internals (Integer(..))
import GHC.Integer.GMP.Prim (int2Integer#)
import GHC.Prim (Int#, (/=#), (>=#), (<#), (-#),
                 int2Word#, word2Int#, popCnt#,
                 negateInt#, and#, or#, xor#, uncheckedIShiftL#)

import GHC.Integer.GMP.PrimExt (popCountInteger#, testBitInteger#,
                                setBitInteger#, clearBitInteger#)

popCountInteger :: Integer -> Int#
popCountInteger (S# i)   = word2Int# (popCnt# (int2Word# i))
popCountInteger (J# s d) = popCountInteger# s d
{-# NOINLINE popCountInteger #-}

testBitInteger :: Integer -> Int# -> Bool
testBitInteger (S# j) i
    | i <# 0# = False
    | i <# (WORD_SIZE_IN_BITS# -# 1#) =
        let !mask = 1# `uncheckedIShiftL#` i in
        word2Int# (int2Word# j `and#` int2Word# mask) /=# 0#
    | otherwise =
        let !(# s, d #) = int2Integer# j in testBitInteger (J# s d) i
testBitInteger (J# s d) i = testBitInteger# s d i /=# 0#
{-# NOINLINE testBitInteger #-}

setBitInteger :: Integer -> Int# -> Integer
setBitInteger (S# j) i
    | i <# 0# = S# j
    | i <# (WORD_SIZE_IN_BITS# -# 1#) =
        let !mask = 1# `uncheckedIShiftL#` i in
        S# (word2Int# (int2Word# j `or#` int2Word# mask))
    | otherwise =
        let !(# s, d #) = int2Integer# j in setBitInteger (J# s d) i
setBitInteger (J# s d) i =
    let !(# s', d' #) = setBitInteger# s d i in J# s' d'
{-# NOINLINE setBitInteger #-}

clearBitInteger :: Integer -> Int# -> Integer
clearBitInteger (S# j) i
    | i <# 0# || i >=# (WORD_SIZE_IN_BITS# -# 1#) = S# j
    | otherwise =
        let !mask =
                int2Word# (1# `uncheckedIShiftL#` i) `xor#`
                int2Word# (negateInt# 1#)
        in S# (word2Int# (int2Word# j `and#` mask))
clearBitInteger (J# s d) i =
    let !(# s', d' #) = clearBitInteger# s d i in J# s' d'
{-# NOINLINE clearBitInteger #-}
