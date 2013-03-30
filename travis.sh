#! /bin/bash

set -e

cabal configure --enable-tests --enable-benchmarks
cabal build
cabal test

cabal bench --benchmark-option='--output=$benchmark.html'

cabal configure
cabal install hscolour
cabal haddock --hyperlink-source --html-location='http://hackage.haskell.org/packages/archive/$pkg/$version/doc/html'

exec > /dev/null 2>&1

git config --global user.name "Travis CI"
git config --global user.email "ci+bitset@knsd.net"

git clone https://${GH_TOKEN}@github.com/superbobry/bitset.git

cd bitset
git checkout -b gh-pages origin/gh-pages

rm -f index.html
mv ../*.html index.html
git add index.html

rm -rf docs
mv ../dist/doc/html/bitset docs
git add docs

git commit -m "Travis build $TRAVIS_BUILD_NUMBER"
git push
