-- Pattern match (pattern matching on function parameters)
-- Pattern matching in function definitions is syntactic sugar for case expressions
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
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pfft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = w / h ^ 2
        (skinny, normal, fat)  = (18.5, 25.0, 30.0) 
        
-- Replaced to Where bindings--
calcBmi :: (RealFloat a) => a -> a -> a
calcBmi weight height = weight / height ^ 2
-- bmiTell 85 1.90

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
{- 
calcBmis' [(85, 1.90), (85, 1.90), (85, 1.843)] 
-}

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname
        
initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."
initials' _ _ = "Please supply two Strings."

{-
Not only can we call functions as infix with backticks, we can also define them using backticks. Sometimes it's easier to read that way.
-}
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b 
  | a > b = GT
  | a == b = EQ
  | otherwise = LT


-- Let Bindings
-- r: radius h: height
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

{-
[let square x = x * x in (square 5, square 3, square 2)]
> [(25,9,4)]
-}

{-
Case expressions
Whereas pattern matching on function parameters can only be done when defining functions, case expressions can be used pretty much anywhere. For instance:
-}

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [_] -> "a singleton list."
                                               _ -> "a longer list."


describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  -- Below is pattern matching in function definition.
  where what [] = "empty."
        what [_] = "a singleton list."
        what _ = "a longer list."