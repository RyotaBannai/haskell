module Ph2.MiscCodes where

-- tail without exception when empty list
safetail :: [a] -> [a]
safetail [] = []
safetail (x : xs) = xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' xs = if null xs then [] else tail xs

-- take third element in the list
third :: [a] -> a
third xs = xs !! 2

third' :: [a] -> a
third' = head . tail . tail

third'' :: [a] -> a
third'' (_ : _ : x : _) = x

-- Luhn algorithm
luhnDouble :: (Ord p, Num p) => p -> p
luhnDouble x = if doubled > 9 then doubled -9 else doubled
  where
    doubled = x * 2

firsts :: [(a, b)] -> [a]
firsts xs = [fst x | x <- xs]

-- apply luhnDoulbe to even index from right
luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = remain == 0
  where
    zipped = zipWith (\i x -> (if even i then (i, luhnDouble x) else (i, x))) [1 ..] [z, y, x, w]
    remain = (`mod` 10) . sum . firsts $ zipped