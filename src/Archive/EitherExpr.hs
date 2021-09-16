module Archive.EitherExpr where

import Data.Either

{-
|<https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Either.html>
-}
r :: Either String Int
r = Right 1234

l :: Either String Int
l = Left "left"

{-
* either := apply the `length function` (if we have a String) or the "times-two" function (if we have an Int)
-}

eApply :: Either [a] Int -> Int
eApply = either length (* 2)

test :: Int
test = eApply l

test' :: Int
test' = eApply r

vals :: [Either [Char] Int]
vals = [Right 1, Left "err1", Right 2, Left "err2"]

-- test2 = ["err1","err2"]
test2 :: [String]
test2 = lefts vals

-- test2' = [1,2]
test2' :: [Int]
test2' = rights vals

-- test2'' = (["err1","err2"],[1,2])
test2'' :: ([String], [Int])
test2'' = partitionEithers vals

-- test3 = Left "err" -- Nothing changes
test3 :: Either String Int
test3 = (* 3) <$> Left "err"