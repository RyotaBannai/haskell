-- Pattern match
-- When defining multiple line in ghci, wrap functions lke this :{ [ function definition comes here. ]:}
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe _ = "Not between 1 and 3" -- catch-all pattern must comes last. Order matters.

-- Recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1 
factorial x = x * factorial (x - 1)

-- Destructing.
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a) -- accepts two tuples.
addVectors (f1, s1) (f2, s2) = (f1 + f2, s1 + s2)

first :: (a, b, c) -> a
first (a, _, _) = a 

head' :: [a] -> a 
head' [] = error "Can't call had on an empty list, dummy!" -- error function takes string and generates runtime error
head' (x:_) = x

-- length of list.
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:b) = length' b + 1


-- Guards 
{-
If all the guards of a function evaluate to False (and we haven't provided an otherwise catch-all guard), evaluation falls through to the next pattern. That's how patterns and guards play nicely together.
-}
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
  | calcBmi w h <= 18.5 = "You're underweight, you emo, you!"
  | calcBmi w h <= 25.0 = "You're supposedly normal. Pfft, I bet you're ugly!"
  | calcBmi w h <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"

calcBmi :: (RealFloat a) => a -> a -> a
calcBmi weight height = weight / height ^ 2
-- bmiTell 85 1.90

{-
Not only can we call functions as infix with backticks, we can also define them using backticks. Sometimes it's easier to read that way.
-}
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b 
  | a > b = GT
  | a == b = EQ
  | otherwise = LT