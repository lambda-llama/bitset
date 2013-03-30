import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

import qualified Data.BitSet as BS

main :: IO ()
main = print $ BS.map (+ 42) $ BS.fromList elems where
  r     = mkStdGen 42
  n     = 128
  elems = shuffle' [1..n] n r
