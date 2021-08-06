module Randomness where

import System.Random

-- we don't have to `random gen :: (Bool, StdGen)` because we do type declaration.
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

-- threeCoins (mkStdGen 21)

-- Return random N numbers
finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, lastGen) = finiteRandoms (n - 1) newGen
   in (value : restOfList, lastGen)

-- finiteRandoms 10 (mkStdGen 10) :: ([Float], StdGen)