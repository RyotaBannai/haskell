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


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' i _         -- if i <= 0
  | i <= 0 = []
take' _ [] = []   -- if list is empty
take' i (x:xs) = x:take' (i-1) xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs

-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs)
--   | x < head xs = quicksort xs
--   | otherwise = head xs: quicksort (x: tail xs)

-- Quicksort. an element that you compare against is called a pivot.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

-- Using filter instead of `list comprehension`
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
  let smallerSorted = quicksort' (filter (<=x) xs)
      biggerSorted = quicksort' (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted
{-
Often the edge case value turns out to be an identity. 
The identity for multiplication is 1 because if you multiply something by 1, you get that something back. 
Also when doing sums of lists, we define the sum of an empty list as 0 and 0 is the identity for addition.
In quicksort, the edge case is the empty list and the identity is also the empty list, 
because if you add an empty list to a list, you just get the original list back.
-}
