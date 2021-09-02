module Pih.Ballots where

import qualified Data.List as List
import Pih.Common (Bit, bin2int, count, int2bit, unfold)

-- simple voting system
votes :: [String]
votes = ["Red", "Blue", "Green", "Red", "Blue", "Blue"]

countVote :: Eq a => a -> [a] -> Int
countVote x = length . filter (== x)

-- same as `nub`
-- rmdups :: Eq a => [a] -> [a]
-- rmdups [] = []
-- rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- [(1,"Green"),(2,"Red"),(3,"Blue")]
result :: Ord b => [b] -> [(Int, b)]
result vs = List.sort [(count v vs, v) | v <- List.nub vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Yet another simple voting system := 第一の選択として名前を書かれた投票が最低だった候補者を取り除く. これを候補者が１人になるまで繰り返す.

ballots :: [[String]]
ballots = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [c] -> c
  (c : cs) -> winner' (elim c bs)
