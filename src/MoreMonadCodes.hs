module MoreMonadCodes where

isBigBang :: (Ord a, Num a) => a -> (Bool, String)
isBigBang x = (x > 9, "Compared gang size to 9.")

get16UsdIfTrue :: (Ord a1, Num a1, Num a2) => a1 -> (Bool, [a2])
get16UsdIfTrue x = let result = x > 5 in (result, if result then [1, 5, 10] else [])

-- takes (a, log) and joined together with the log the value that results from the funciton (f x)
-- (3, "Smallish gang.") `applyLog` isBigBang # (False, "Smallish gang.Compared gang size to 9.")
-- a := a `value`, and String is a `log`.
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- (3, "Smallish gang.") `betterApplyLog` isBigBang # (False, "Smallish gang.Compared gang size to 9.")
-- (3, []) `betterApplyLog` get16UsdIfTrue # (False,[1,5,10])
betterApplyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
betterApplyLog (x, log) f = let (y, newLog) = f x in (y, log <> newLog)