module Archive.SeqExpr where

import qualified Data.Sequence as S

{-
| <https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Sequence.html#g:3>
-}

testSeq :: S.Seq Int
testSeq = S.update 2 3 $ S.empty S.|> 5 S.|> 20 S.|> 30

testSeq2 :: S.Seq String
testSeq2 = S.fromList ["alligator", "monkey", "zebra"]

-- test = Just 3
test :: IO ()
test = print $ S.lookup 2 testSeq

{-
* (?!) := infix version of `lookup`
-}
test' :: IO ()
test' = print $ testSeq S.!? 2

-- Exception when indexing is out of index
test'' :: IO ()
test'' = print $ testSeq `S.index` 5

-- test2 = 5 :< fromList [20,3]
test2 :: S.ViewL Int
test2 = S.viewl testSeq -- viewr

-- test3 = fromList [5,25,28] -- each Seq value is accumuated by each steps.
test3 :: S.Seq Int
test3 = S.scanl1 (+) testSeq

-- test4 = fromList [5,20,3] :< fromList [fromList [20,3],fromList [3],fromList []]
test4 :: S.ViewL (S.Seq Int)
test4 = S.viewl $ S.tails testSeq

{-
* span := takes values until Predicate satisfies and put right element in tuple, and leave the rest as second element in tuple

* breakl p is equivalent to spanl (not . p).
-}

-- test5 = (fromList [5,20,3],fromList [])
test5 :: (S.Seq Int, S.Seq Int)
test5 = S.spanl (< 100) testSeq

test6 :: S.Seq Int
test6 = S.sort testSeq

{-
* (><) :=  Concatenate two sequences.
-}
-- If, instead, `sortBy` had been used, length would be evaluated on every comparison,
test6' :: S.Seq String
test6' = S.sortOn length testSeq2

insertAt :: Int -> a -> S.Seq a -> S.Seq a
insertAt i x xs = S.take i xs S.>< S.singleton x S.>< S.drop i xs

-- test7 = fromList [5,100,20,3]
test7 :: S.Seq Int
test7 = insertAt 1 100 testSeq -- Sequence provides `insertAt`

-- calc averate without doing `sum / length list`
average :: [Double] -> Double
average = S.foldlWithIndex (\a i x -> let k = fromIntegral i in (k * a + x) / (k + 1)) 0 . S.fromList

-- test8 = 2.5
test8 :: Double
test8 = average [1 .. 4]