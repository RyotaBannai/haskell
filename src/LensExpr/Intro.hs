{-# LANGUAGE TemplateHaskell #-}

module LensExpr.Intro where

import Control.Lens

type Degrees = Double

type Latitude = Degrees

type Longitude = Degrees

data Meetup = Meetup {_name :: String, _location :: (Latitude, Longitude)}

-- makeLenses ''Meetup

re :: [Char]
re = ("hello", ("world", "!!!")) ^. _2 . _1

-- (.~) is an infix alias for set.
re2 :: ([Char], [Char])
re2 = _1 .~ "hello" $ ((), "world")

items :: Traversable t => Traversal' (t a) a
items = traverse

re3 :: [Integer]
re3 = toListOf items [1 .. 3]

{-
"Checking 1"
"Checking 3"
"Checking 4"
Just 4
-}
re4 :: IO (Maybe Integer)
re4 = findMOf each (\x -> print ("Checking " ++ show x) >> return (even x)) (1, 3, 4, 6)

re5 :: [String]
re5 = traverse <>~ "!!" $ map show [1, 2, 3]

-- Lens changed operations
re5' :: [String]
re5' = [1, 2, 3] & over mapped show & traverse <>~ "!!"

re5'' :: [String]
re5'' = map show [1, 2, 3] & traverse <>~ "!!"