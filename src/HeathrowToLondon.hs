module HeathrowToLondon where

{-
$ cat resources/paths.txt | cabal ru
-}

data Node = Node Road Road | EndNode Road -- Node has two Road or only load to accross the road to get to the opposite.

data Road = Road Int Node -- Road has how long it is and the Node it connects to.

-- RoadSystem := an algebraic data type := a kind of composite type, i.e., a type formed by combining other types.
-- It's better to use new type, then we can't accidentally add different type to another
data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)

-- Type synonym
type RoadSystem = [Section]

-- RoadSystem from Heathrow to London
heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int)]

-- Calculate new best paths pair to points (`AX`, `BX`)
roadStep :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)
roadStep (pathA, pathB, priceA, priceB) (Section a b c) =
  let forwardPriceToA = priceA + a
      crossPriceToA = priceA + b + c
      forwardPriceToB = priceB + b
      crossPriceToB = priceA + a + c
      (newPathToA, newPriceA) =
        if forwardPriceToA <= crossPriceToA
          then ((A, a) : pathA, forwardPriceToA)
          else ((C, c) : (B, b) : pathB, crossPriceToA)
      (newPathToB, newPriceB) =
        if forwardPriceToB <= crossPriceToB
          then ((B, b) : pathB, forwardPriceToB)
          else ((C, c) : (A, a) : pathA, crossPriceToB)
   in (newPathToA, newPathToB, newPriceA, newPriceB)

optimalPath :: RoadSystem -> (Path, Int)
optimalPath roadSystem =
  let (bestAPath, bestBPath, ofAPrice, ofBPrice) = foldl roadStep ([], [], 0, 0) roadSystem
   in if ofAPrice <= ofBPrice
        then (reverse bestAPath, ofAPrice)
        else (reverse bestBPath, ofBPrice)

-- groupsOf 3 [1,2,3,4,5,6] >> [[1,2,3],[4,5,6]]
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

calcBestPath :: IO ()
calcBestPath = do
  contents <- getContents
  let threeRoads = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) threeRoads
      (path, pathPrice) = optimalPath roadSystem
      --  map fst [(A, 1),(A, 1),(A, 1)] >> AAA
      pathString = concatMap (show . fst) path -- concat $ map
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show pathPrice