bitset  [![Build Status][travis-img]][travis]
======

A _bit set_ is a compact data structure, which maintains a set of members
from a type that can be enumerated (i. e. has an `Enum` instance). Current
implementation is abstract with respect to conatiner type, so any
numeric type with `Bits` instance can be used as a container. However,
independent of container choice, the maximum number of elements in a
bit set is bounded by `maxBound :: Int`.

Here's a usage example for a dynamic bit set, which uses `Integer` for
storing bits:

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
