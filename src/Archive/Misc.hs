{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Archive.Misc where

import Control.Arrow
import Criterion.Main
import Data.Array.IArray
import Data.List.NonEmpty hiding (map, take)
import Data.Semigroup
import System.Random

{-
minmax performance : <https://stackoverflow.com/questions/49991440/simultaneous-minimum-and-maximum-of-a-list>
-}

bounds1, bounds2 :: (Bounded a, Ord a) => [a] -> (a, a)
bounds1 = foldMap (Min &&& Max) >>> getMin *** getMax
bounds2 xs = (minimum xs, maximum xs)

randomList :: Int -> IO [Int]
randomList count = take count . randoms <$> newStdGen

mkBench :: Int -> Benchmark
mkBench n = env (randomList n) $ \list ->
  bgroup
    (show n)
    [ bench "foldMap" $ nf bounds1 list,
      bench "minmax" $ nf bounds2 list
    ]

performanceTest :: IO ()
performanceTest = defaultMain $ map mkBench [100, 1000, 10000]

{-
  | <https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List-NonEmpty.html#g:1>
Advent of code | <https://github.com/walerian777/advent-of-code/blob/f6a51a98db7edd2630cc68bade93fc6f1b7c94a8/2018/day-17/ReservoirResearch.hs>

* (:|) := Non-empty (and non-strict) list type.

λ nonEmpty []     # Nothing
λ nonEmpty [1..3] # Just (1 :| [2,3])

* toList, fromList

-}
-- test2 = "Hello Haskell!"
test2 :: [Char]
test2 = sconcat $ "Hello" :| [" ", "Haskell", "!"] -- same as `<>` but being used for better performance.

debugg :: (Ord b, Ord b', Enum b') => NonEmpty (b, b') -> ((Min b, Min b'), (Max b, Max b'))
debugg xs = sconcat (((Min *** Min . pred) &&& (Max *** Max . succ)) <$> xs)

-- test3 = ((Min {getMin = 0},Min {getMin = -1}),(Max {getMax = 100},Max {getMax = 101}))
test3 :: ((Min Integer, Min Integer), (Max Integer, Max Integer))
test3 = debugg $ fromList [(0, 0), (100, 100)]

-- * Learn how `Semigroup`'s `Max` and `Min` and other type works when it's concatenated.

-- test3' :: NonEmpty ((Min Integer, Min Integer), (Max Integer, Max Integer))
-- test3' = debugg $ fromList [(0, 0), (100, 100)]

-- a :: (IArray a e, Ix e, Num e, Enum e) => a e e
-- a = array (1, 100) ((1, 1) : [(i, i * a ! (i -1)) | i <- [2 .. 100]])

-- λ hist (1,100) [25..50]
hist :: (Ix a, Num b) => (a, a) -> [a] -> Array a b
hist bnds is = accumArray (+) 0 bnds [(i, 1) | i <- is, inRange bnds i]

-- λ test4 # [(1,"OK"),(2,"OK"),(3,"OK"),(4,"OK")]
test4 :: [(Integer, [Char])]
test4 = (,"OK") <$> [1 .. 4]