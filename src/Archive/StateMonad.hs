{-# LANGUAGE LambdaCase #-}

module Archive.StateMonad where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.Functor.Identity
import Data.Maybe

{-
| Also check out <src/Lyah/MoreMonadCodes.hs>

type State s = StateT s Identity
-}

addArg :: (Monad m, Num a) => a -> StateT a m a
addArg x = state $ \s -> (x, s + x)

addOddArg :: (Monad m, Integral a) => a -> StateT a m a
addOddArg x = state $ \s -> if odd x then (x, s + x) else (x, s)

addOddArg' :: (Monad m, Integral a) => a -> StateT a m (Maybe a)
addOddArg' x = state $ \s -> if odd x then (Just x, s + x) else (Nothing, s)

-- 9 goes to `s`
-- test = ([1,2,3,4],19)
test :: ([Integer], Integer)
test = runState (mapM addArg [1 .. 4]) 9

-- λ test' 10 [1..3] # ([1,2,3],14)
test' :: (Traversable t, Integral s) => s -> t s -> (t s, s)
test' s args = runState (mapM addOddArg args) s

-- λ test'' 10 [1..3] # ([Just 1,Nothing,Just 3],14)
test'' :: (Integral s) => s -> [s] -> ([Maybe s], s)
test'' s args = runState (mapM addOddArg' args) s

-- λ convMaybe' [Just 1,Nothing] # [1]
convMaybe :: [Maybe a] -> [a]
convMaybe = map fromJust . filter isJust

convMaybe' :: [Maybe Integer] -> [Integer]
convMaybe' = catMaybes -- same as above.

-- λ comb' 10 [1..3] # ([1,3],14)
comb :: Integer -> [Integer] -> ([Integer], Integer)
comb s args = _1 %~ catMaybes $ test'' s args
