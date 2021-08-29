module Lyah.TypesAndTypeclasses where

-- type declaration
removeNonUpperCase :: String -> String
-- removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z