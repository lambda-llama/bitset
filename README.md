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
main = print $ bs \\ BitSet.singleton Blue where
  bs :: BitSet Color
  bs = BitSet.fromList [Red, Green, Blue]
```

Benchmarks
----------

To run benchmarks, create a separate `cabal-dev` environment in `benchmarks/`
directory and:

```bash
$ cd ./benchmarks
$ cabal-dev install-deps && cabal-dev build
$ ./dist/build/bitset-benchmarks/bitset-benchmarks -o dist/bench.html
```

[travis]: http://travis-ci.org/superbobry/bitset
[travis-img]: https://secure.travis-ci.org/superbobry/bitset.png
