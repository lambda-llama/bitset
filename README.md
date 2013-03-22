bitset  [![Build Status][travis-img]][travis]
======

A /bit set/ is a compact data structure, which maintains a set of members
from a type that can be enumerated (i. e. has an `Enum` instance). Current
implementations uses `Integer` for as bit storage and provides most of the
expected set operations: insertion, deletion, intersection, membership
testing etc.

Example
-------

```haskell
import Data.BitSet (BitSet, (\\))
import qualified Data.BitSet as BitSet

data Color = Red | Green | Blue deriving (Show, Enum)

main :: IO ()
main = print $ bs \\ BitSet.fromList [Blue] where
  bs :: BitSet Color
  bs = BitSet.formList [Red, Green, Blue]
```

[travis]: http://travis-ci.org/superbobry/bitset
[travis-img]: https://secure.travis-ci.org/superbobry/bitset.png
