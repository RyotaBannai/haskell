module FunctorCodes where

import Control.Applicative
import Data.Char
import Data.List

-- reverse text
simple :: IO ()
simple = do
  line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

-- Like we can apply a function to something that's inside a Maybe box, we can apply a function to what's inside an `IO box`, only it has to go out into the real world to get something.

-- using fmap
-- fmap :: (a -> b) -> IO a -> IO b
simple' :: IO ()
simple' = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

-- anc >> C-B-A
moreComplex :: IO ()
moreComplex = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

{-
Definition of Applicative:
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- if we want to mkae a type contructor part of the Applicative typeclass,
  it has to be in Functor first.
-- pure := takes a value of any type and return an applicative functor with that value inside it.
-- (<*>)
  := takes a `functor that has a function in it` and `another functor` and sort of extracts that function `from the first functor` and then `maps it over the second one`
  := <*> is left-associative, so `pure (+) <*> Just 3 <*> Just 5` is the same as `(pure (+) <*> Just 3) <*> Just 5`
-}

-- Just concatenation of two strings in Applicative context
myIOAction :: IO String
myIOAction = (++) <$> getLine <*> getLine

-- Functions as Applicative functor
-- applyToAllFunctions 5 >> 508
applyToAllFunctions :: Integer -> Integer
applyToAllFunctions = (+) <$> (+ 3) <*> (* 100)

-- applyToAllFunctions' 5 >> [8.0, 10.0, 2.5]
applyToAllFunctions' :: Double -> [Double]
applyToAllFunctions' = (\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) <*> (/ 2)

sampleZipList :: [Integer]
sampleZipList = getZipList $ (*) <$> ZipList [1, 2, 3] <*> ZipList [1, 5, 10]

-- [('d', 'c', 'r'), ('o', 'a', 'a'), ('g', 't', 't')]
sampleZipList' :: [(Char, Char, Char)]
sampleZipList' = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"

-- liftA2 := takes two functors and a fucntion, and applys the function on the two functors, and return the result of functor.
-- Takes `a list of Functors` and return `a Functor of a list`
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

-- Or
-- sequenceA = foldr (\x -> (<*>) ((:) <$> x)) (pure [])

{-
sequenceA [[1,2],[3,4]]
>> (:) <$> [1,2] <*> sequenceA [[3,4]]
>> (:) <$> [1,2] <*> [[3],[4]]  -- [3:[],4:[]] with an edge case pure
>> [1:[3],1:[4],2:[3],3:[4]]
>> [[1,3],[1,4],[2,3],[2,4]]
-}
-- Easier version to understand the codes
-- sequenceA [] = pure []
-- sequenceA (x : xs) = (:) <$> x <*> sequenceA xs

{-
‘Prelude.sequenceA’ has the same method
(and originally defined in ‘Data.Traversable’
-}

-- use `newtype` to get the ability to apply function on only specific component of tuple:
newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- getPair $ fmap (*100) (Pair (2,3)) >> (200, 3)
-- getPair $ fmap reverse  (Pair ("los angeles",3)) >> ("selegna sol",3)