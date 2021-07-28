multThree :: Num a => a -> a -> a -> a
multThree x y z = x * y * z

-- Partially applied functions
multTwoWithNine = multThree 9
multWithEighteen = multTwoWithNine 2

{-
type Ordering :: *
data Ordering = LT | EQ | GT
-- Defined in ‘GHC.Types’
-}
-- Return LT | EQ | GT
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x  -- returns function that takes two values

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100 

divideByTen :: (Floating a) => a -> a
divideByTen = (/10) -- wrap with parentheses
                    -- is equivalent to doing 200 / 10, as is doing (/10) 200

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f(f x)
-- applyTwice (+3) 10

{-
If our function requires us to pass it a function that takes only one parameter, 
we can just partially apply a function to the point where it takes only one parameter and then pass it.

applyTwice (+3) 10  
16  
applyTwice (++ " HAHA") "HEY"  
"HEY HAHA HAHA"  
applyTwice ("HAHA " ++) "HEY"  
"HAHA HAHA HEY"  
applyTwice (multThree 2 2) 9  
144  
applyTwice (3:) [1]
[3,3,1]  
-}

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys
-- zipWith' (+) [1,2,3,4] [1,2,3,4]
-- zipWith' max [1,3,5,7] [2,4,6,8]
-- zipWith' (++) ["abc", "efg"] ["hij", "kln"]
-- zipWith' (*) (replicate 5 2) [1..]  // [1..] := [1,2,3,4,...]
-- zipWith' (zipWith' (*))
{-
zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]

f := zipWith' (*)
x:xs := [1,2,3]:[[3,5,6],[2,3,4]]
y:ys := [3,2,2]:[[3,4,5],[5,4,3]]
-}

-- zipWith (flip div) [2,2..] [10,8,6,4,2]
-- map (map (^2)) [[1,3], [2,3,5,2], [4,4]]

filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)                   -- 'p' stands for `predicate`
    | p x       = x : filter' p xs -- if p x returns True, then add it to result list.
    | otherwise = filter' p xs
    
-- let notNull x = not (null x) in filter notNull [[1,2,3], [], [3,4,5], [], [7,8,9]]
-- null [] := True

-- filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent" // the predicate takes first arg as Char from the string