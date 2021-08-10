module MoreMonadCodes where

isBigBang :: (Ord a, Num a) => a -> (Bool, String)
isBigBang x = (x > 9, "Compared gang size to 9.")

-- takes (a, log) and joined together with the log the value that results from the funciton (f x)
-- (3, "Smallish gang.") `applyLog` isBigBang # (False, "Smallish gang.Compared gang size to 9.")
-- a := a `value`, and String is a `log`.
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)