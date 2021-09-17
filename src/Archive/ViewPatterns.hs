{-# LANGUAGE ViewPatterns #-}

module Archive.ViewPatterns where

import qualified Data.List as List

{-
| <https://gitlab.haskell.org/ghc/ghc/-/wikis/view-patterns#view-patterns-lightweight-views-for-haskell>
-}

{-
There is no new form of declaration (e.g. 'view' or 'pattern synonym').
-}

{-
* View Patterns
-}

type Typ = (Int, Int)

data TypView = Unit | Arrow Int Int

view' :: Typ -> TypView
view' t
  | f == s = Unit
  | otherwise = Arrow f s
  where
    f = fst t
    s = snd t

hypotenuse :: Typ -> Int
hypotenuse t = case view' t of
  Unit -> 0
  Arrow f s -> (f * 2 + s * 2) `div` 2

-- View Patterns
hypotenuse' :: Typ -> Int
hypotenuse' (view' -> Unit) = 0
hypotenuse' (view' -> Arrow f s) = (f * 2 + s * 2) `div` 2

lenFour :: (String -> Int) -> String -> Bool
lenFour f (f -> 4) = True

-- Variables can be bound to the left in tuples and data constructors:
lenFour' :: ((String -> Int, Int), String) -> Bool
lenFour' ((f, _), f -> 4) = True

data JList a = Empty | Single a | Join (JList a) (JList a) deriving (Show)

data JListView a = Nil | Cons a (JList a) deriving (Show)

-- λ view $ Join(Join (Single 1) (Single 1)) (Single 1) # Cons 1 (Join (Join Empty (Single 1)) (Single 1))
view :: JList a -> JListView a
view Empty = Nil
view (Single a) = Cons a Empty
view (Join (view -> Cons xh xt) y) = Cons xh $ Join xt y
view (Join (view -> Nil) y) = view y

-- λ len $ Join( Join (Single 1) (Single 1)) (Single 1) # 3
len :: JList a -> Int
len (view -> Nil) = 0
len (view -> Cons x xs) = 1 + len xs

{-
* Partial Views
-}

newtype Set a = S [a] deriving (Show)

{-
λ has 2 $ S([1..2])    # Just (S [1])
λ delete 2 $ S([1..2]) # S [1]
λ insert 2 $ S([1..2]) # S [1,2]
-}

has :: Eq a => a -> Set a -> Maybe (Set a)
has x (S xs)
  | x `elem` xs = Just $ S (List.delete x xs)
  | otherwise = Nothing

delete :: Eq a => a -> Set a -> Set a
delete x (has x -> Just s) = s
delete x s = s

insert :: Eq a => a -> Set a -> Set a
insert x s@(has x -> Just _) = s
insert x (S xs) = S (x : xs)

{-
* N+K patterns
-}

np :: (Ord a, Num a) => a -> a -> Maybe a
np k n
  | k <= n = Just (n - k)
  | otherwise = Nothing

fib :: (Num t, Num p, Ord t) => t -> p
fib 0 = 1
fib 1 = 1
fib (np 2 -> Just n) = fib (n + 1) + fib n

{-
* Implicit feature is not yet available
-}

-- We may implement a special syntax that makes the Just implicit, using expr => pat for expr -> Just pat
-- fib' :: (Num t, Num p, Ord t) => t -> p
-- fib' 0 = 1
-- fib' 1 = 1
-- fib' (np 2 => n) = fib' (n + 1) + fib' n

{-
* Named constants
View patterns can be used to pattern match against named constants:
    errorVal :: Int -> Bool
    errorVal = (== 4)
    f (errorVal -> True) =  ...
-}