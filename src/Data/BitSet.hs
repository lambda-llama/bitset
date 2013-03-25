-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BitSet.Dynamic
-- Copyright   :  (c) Sergei Lebedev, Aleksey Kladov 2013
--                Based on Data.BitSet (c) Denis Bueno 2008-2009
-- License     :  MIT
-- Maintainer  :  superbobry@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- A space-efficient implementation of set data structure enumerated
-- data types.
--
-- /Note/: Read below the synopsis for important notes on the use of
-- this module.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import Data.BitSet (BitSet)
-- > import qualified Data.BitSet as BS
--
-- This module re-exports 'Dynamic' implementation that uses
-- 'Integer' as underlying container.

module Data.BitSet ( module Data.BitSet.Dynamic ) where

import Data.BitSet.Dynamic
