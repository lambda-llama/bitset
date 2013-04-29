{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Integer.GMP.PrimExt
    ( popCountInteger#
    , testBitInteger#
    , setBitInteger#
    , clearBitInteger#
    ) where

import GHC.Prim (Int#, ByteArray#)

foreign import prim "integer_cmm_popCountIntegerzh" popCountInteger#
  :: Int# -> ByteArray# -> Int#

foreign import prim "integer_cmm_testBitIntegerzh" testBitInteger#
  :: Int# -> ByteArray# -> Int# -> Int#

foreign import prim "integer_cmm_setBitIntegerzh" setBitInteger#
  :: Int# -> ByteArray# -> Int# -> (# Int#, ByteArray# #)

foreign import prim "integer_cmm_clearBitIntegerzh" clearBitInteger#
  :: Int# -> ByteArray# -> Int# -> (# Int#, ByteArray# #)
