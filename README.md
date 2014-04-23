bitset  [![Build Status][travis-img]][travis]
======

A _bit set_ is a compact data structure, which maintains a set of members
from a type that can be enumerated (i. e. has an `Enum` instance). Current
implementation is abstract with respect to conatiner type, so any
numeric type with `Bits` instance can be used as a container.

Here's a usage example for a dynamic bit set, which uses a tweaked version
of `Integer` for storing bits:

```haskell
import Data.BitSet (BitSet, (\\))
import qualified Data.BitSet as BitSet

data Color = Red | Green | Blue deriving (Show, Enum)

main :: IO ()
main = print $ bs \\ BitSet.singleton Blue where
  bs :: BitSet Color
  bs = BitSet.fromList [Red, Green, Blue]
```

The API is exactly the same for a static bitset, based on native `Word`.
Here's an example from [`hen`] [hen] library, which uses `Data.BitSet` to
store Xen domain status flags:

```haskell
import qualified Data.BitSet.Word as BS

data DomainFlag = Dying
                | Crashed
                | Shutdown
                | Paused
                | Blocked
                | Running
                | HVM
                | Debugged
    deriving (Enum, Show)

isAlive :: DomainFlag -> Bool
isAlive = not . BS.null . BS.intersect (BS.fromList [Crashed, Shutdown])
```

Benchmarks
----------

To run benchmarks, configure `cabal` with benchmarks
and build:

```bash
$ cabal-dev install-deps --enable-benchmarks && cabal-dev build
$ ./dist/build/bitset-benchmarks/bitset-benchmarks -o dist/bench.html
```

[travis]: http://travis-ci.org/lambda-llama/bitset
[travis-img]: https://secure.travis-ci.org/lambda-llama/bitset.png
[hen]: https://github.com/selectel/hen/
