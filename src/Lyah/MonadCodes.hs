module Lyah.MonadCodes where

import Control.Monad

-- Just 3 `applyMaybe` \x -> Just (x+1) # Just 4
applyMaybe :: Maybe t -> (t -> Maybe a) -> Maybe a
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- Pierre's balance pole with birds
type Birds = Int

type Pole = (Birds, Birds)

-- landLeft 2 (landRight 1 (landLeft 1 (0,0))) -- enumate birds landing on the pole
landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

-- (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2
(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

-- reworking
-- (0,0) -: landLeft' 10 # Nothing
-- return (0,0) >>= landLeft' 1 >>= landRight' 1 # Just (1,1)
-- Nothing >>= landLeft' 2 # `fail` will be propaged.
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left, right)
  | abs ((left + n) - right) < 4 = Just $ landLeft n (left, right)
  | otherwise = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left, right)
  | abs (left - (right + n)) < 4 = Just $ landRight n (left, right)
  | otherwise = Nothing

-- return (0,0) >>= landLeft' 1 >>= landRight' 1 >>= banana >>= landRight' 1 # Nothing
banana :: Pole -> Maybe Pole
banana _ = Nothing

nestedMonad :: Maybe String
nestedMonad =
  Just 3
    >>= ( \x ->
            Just "!"
              >>= ( \y -> Just (show x)
                  )
        )

-- improved
nestedMonad' :: Maybe String
nestedMonad' = do
  x <- Just 3 -- extract a value from Maybe temporarily
  y <- Just "!"
  Just (show x ++ y)

{-
It's important to remember that `do` expressions are just different syntax for chaining monadic values.
-}

-- tightwalker's routine
-- # Just (3,2)
routine :: Maybe Pole
routine = do
  let start = (0, 0)
  first <- landLeft' 2 start
  -- Nothing # this is equivalent to `banana`
  second <- landRight' 2 first
  landLeft' 1 second

-- Monadic do expression fails in a pattern matching, but return value in `fail` method # Nothing
wopwop :: Maybe Char
wopwop = do
  (x : xs) <- Just ""
  return x

{-
If `gaurd` succeeds, teh result contained within it is an empty tuple. So then, we use `>>` to ignore that empty  tuple and present something else as the result.
However, if guard fails, then so will the `return` later on, because feeding an empty list to a fucntion with `>>=` always results in an empty list.
-}
getOnlyContains7 :: [Integer]
getOnlyContains7 = [1 .. 50] >>= (\x -> guard ('7' `elem` show x) >> return x)

-- Monad functions composition
f :: Num a => a -> [a]
f x = [x, - x]

g :: Num a => a -> [a]
g x = [x * 3, x * 2]

-- the order of apply Monad functions shouldn't matter.
-- h 3 # [9,-9,6,-6]
h :: Integer -> [Integer]
h = f <=< g