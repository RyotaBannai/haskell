module Archive.MonadExpr where

import Control.Monad
import Data.Maybe
import Text.Read (readMaybe)

{-
Also check | <src/Lyah/MoreMonadCodes.hs>
Data.Maybe.Unpacked | <https://hackage.haskell.org/package/unpacked-maybe-0.1.0.0/docs/Data-Maybe-Unpacked.html>
filterM | <https://blog.ssanj.net/posts/2018-04-10-how-does-filterm-work-in-haskell.html>
-}

{-
* filterM := list に対し Predicate に基づいてフィルタリングを行った結果を Maybe でラップする.
-}

-- mapMaybe id == catMaybes # λ catMaybes [Just 1, Just 3] # [1,3]

inp :: [Maybe Integer]
inp = map Just [1 .. 3]

numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

-- [Just 1,Just 2,Just 3] # Just [2,3,4]
test :: Maybe [Integer]
test = mapM (fmap (+ 1)) inp

-- test2 = Just [Just 2]
test2 :: Maybe [Maybe Integer]
test2 = filterM (fmap even) inp

-- test3 = Just [2,4]
test3 :: Maybe [Int]
test3 = filterM (Just . (== 100)) numbers

{-
* `mfilter` is a simple helper with someting like `case predicate x of Just True -> x; Just False -> Nothing`
-}

{-
λ mfilter odd (Just 1) # Just 1
λ mfilter odd (Just 2) # Nothing
-}

-- test4 = [Just 1,Just 3]
test4 :: [Maybe Integer]
test4 = filter isJust $ map (mfilter odd) inp