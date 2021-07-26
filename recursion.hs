maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list" -- edge condition
maximum' [x] = x                            -- edge condition: the singleton list, just give back the only element.
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs
{-
maximum' [1,5,23,52,234,23,23,4,23,41,24,2]
-}

-- Using default max function
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "Maximum of empty list"
maximum'' [x] = x                            
maximum'' (x:xs) = max x (maximum'' xs)

-- Num https://en.wikibooks.org/wiki/Haskell/Type_basics_II#The_Num_class
-- The most important numeric types are Int, Integer and Double. you can check by :t (-7) -> (Num a) => a
-- We used guards because we are testing Boolean condition.

{-
NOTE: Num is not a subclass of Ord. That means that what constitutes for a number doesn't really have to adhere to an ordering. 
So that's why we have to specify both the Num and Ord class constraints when doing addition or subtraction and also comparison.
-}
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' i a 
  | i <= 0 = []
  | otherwise = a:replicate' (i-1) a