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