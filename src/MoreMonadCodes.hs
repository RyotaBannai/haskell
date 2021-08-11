module MoreMonadCodes where

import Control.Monad
import Control.Monad.Writer
import Data.Monoid

isBigBang :: (Ord a, Num a) => a -> (Bool, String)
isBigBang x = (x > 9, "Compared gang size to 9.")

get16UsdIfTrue :: (Ord a1, Num a1, Num a2) => a1 -> (Bool, [a2])
get16UsdIfTrue x = let result = x > 5 in (result, if result then [1, 5, 10] else [])

-- takes (a, log) and joined together with the log the value that results from the funciton (f x)
-- (3, "Smallish gang.") `applyLog` isBigBang # (False, "Smallish gang.Compared gang size to 9.")
-- a := a `value`, and String is a `log`.
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- Now Works on any `Monoid`
-- (3, "Smallish gang.") `betterApplyLog` isBigBang # (False, "Smallish gang.Compared gang size to 9.")
-- (3, []) `betterApplyLog` get16UsdIfTrue # (False,[1,5,10])
betterApplyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
betterApplyLog (x, log) f = let (y, newLog) = f x in (y, log <> newLog)

type Food = String

type Price = Sum Int

-- let (_, result) = ("beans", Sum 10) `betterApplyLog` addDrink in getSum result # 35
addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

{-
-- Just wrap of a normal tuple.
newtype Writer w a = Writer {runWriter :: (a, w)}
instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- Few examples:
runWriter (return 3 :: Writer String Int)        # (3,"")
runWriter (return 3 :: Writer (Sum Int) Int)     # (3,Sum {getSum = 0})
runWriter (return 3 :: Writer (Product Int) Int) # (3,Product {getProduct = 1})
-}
