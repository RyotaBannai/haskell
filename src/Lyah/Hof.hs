module Lyah.Hof where

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
compareWithHundred x = compare 100 x -- returns function that takes two values

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10) -- wrap with parentheses
-- is equivalent to doing 200 / 10, as is doing (/10) 200

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

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
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

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
filter' p (x : xs) -- 'p' stands for `predicate`
  | p x = x : filter' p xs -- if p x returns True, then add it to result list.
  | otherwise = filter' p xs

-- let notNull x = not (null x) in filter notNull [[1,2,3], [], [3,4,5], [], [7,8,9]]
-- null [] := True

-- filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent" // the predicate takes first arg as Char from the string

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

firstWord :: [Char]
firstWord = takeWhile (/= ' ') "elephants know how to party"

sumOfAllOddsUnderX :: (Integral x) => x -> x
sumOfAllOddsUnderX n = sum (takeWhile (< n) (filter odd (map (^ 2) [1 ..])))

sumOfAllOddsUnderX' :: (Integral x) => x -> x
sumOfAllOddsUnderX' n = sum (takeWhile (< n) [n ^ 2 | n <- [1 ..], odd (n ^ 2)])

-- Collatz sequences
collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
  | even n = n : collatzChain (n `div` 2)
  | odd n = n : collatzChain (n * 3 + 1)

-- collatzChain 10 // [10,5,16,8,4,2,1]

numLongChains :: Int
numLongChains = length (filter isLong (map collatzChain [1 .. 100]))
  where
    isLong xs = length xs > 15

-- Using Lambdas
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map collatzChain [1 .. 100]))

{-
Note: This function has a type of numLongChains :: Int because length returns an Int instead of a Num a for historical reasons.
If we wanted to return a more general Num a, we could have used fromIntegral on the resulting length.
-}

{-
People who are not well acquainted with how currying and partial application works often use lambdas
where they don't need to. For instance, the expressions map (+3) [1,6,3,2] and map (\x -> x + 3) [1,6,3,2]
are equivalent since both (+3) and (\x -> x + 3) are functions that take a number and add 3 to it
-}

listOfFuns = map (*) [0 ..]

result = (listOfFuns !! 1) 5 -- `!!` means indexing list

-- These are equivalent.
addThree :: (Num a) => a -> a -> a -> a
addThree a b c = a + b + c

addThree' :: (Num a) => a -> a -> a -> a
addThree' = \a -> \b -> \c -> a + b + c

addThree'' :: (Num a) => a -> a -> a -> a
addThree'' = \a b c -> a + b + c

-- Fold
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

{-
some improvements:
sum' = foldl (\acc x -> acc + x) 0
sum' = foldl (+) 0
sum' = foldl (+) 0
sum' = sum
-}

-- check if ys contains y or not.
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

{-
One big difference is that:
right folds work on infinite lists, whereas left ones don't!
To put it plainly, if you take an infinite list at some point and you fold it up from the right,
you'll eventually reach the beginning of the list.
However, if you take an infinite list at a point and you try to fold it up from the left, you'll never reach an end!

-- https://stackoverflow.com/questions/3082324/foldl-versus-foldr-behavior-with-infinite-lists
-}

-- scan generates list of accumulated compuated result.
sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..]))) + 1

-- with Function application & Function composition
-- add $ at the end to apply inputs
sqrtSums' :: Int
sqrtSums' = (+) 1 . length . takeWhile (< 1000) . scanl1 (+) . map sqrt $ [1 ..]

-- More readable?
sqrtSums'' :: Int
sqrtSums'' =
  let takeWhileUnder1000 = takeWhile (< 1000) accumAllWithAddition
      accumAllWithAddition = scanl1 (+) squeresAll
      squeresAll = map sqrt [1 ..]
   in (+) 1 $ length takeWhileUnder1000

{-
We use takeWhile here instead of filter because filter doesn't work on infinite lists.
Even though we know the list is ascending, filter doesn't, so we use takeWhile to cut the scanlist off at the first occurence of a sum greater than 1000.
-}

-- Function application with &
resultOfAppliedEachFunction :: [Double]
resultOfAppliedEachFunction = map ($ 3) [(4 +), (10 *), (^ 2), sqrt]

-- Function composition
negateAll :: [Integer]
negateAll = map (\x -> negate (abs x)) [5, 2, -2, 3, -1, -5]

-- Function composition is right associative
negateAll' :: [Integer]
negateAll' = map (negate . abs) [5, 2, -2, 3, -1, -5]