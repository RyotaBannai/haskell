module Archive.Expr.MonadTrans where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text.Internal.Fusion (Stream)
import System.IO

transMaybeIO :: MaybeT IO (Int -> MaybeT IO Int)
transMaybeIO = return (\x -> return (x + 3) :: MaybeT IO Int) :: MaybeT IO (Int -> MaybeT IO Int)

transMaybeIO' :: MaybeT IO (Int -> Int -> Int)
transMaybeIO' = return (*) :: MaybeT IO (Int -> Int -> Int)

transformedVal :: a -> MaybeT IO a
transformedVal = return

transformedVal' :: MaybeT IO (Int -> Int)
transformedVal' = return (* 3) :: MaybeT IO (Int -> Int)

res :: MaybeT IO (Int -> Int)
res = liftA2 (<*>) transMaybeIO' transformedVal'

--
--
a :: MaybeT IO String
a = return "aa"

b :: MaybeT IO [Char -> Char]
b = return [id, id]

-- https://blog.ssanj.net/posts/2014-08-10-boosting-liftA2.html
-- match expected type ‘MaybeT IO [Char -> Char]’
-- "aaaa"
re :: MaybeT IO String
re = liftA2 (<*>) b a

a' :: MaybeT IO [Int]
a' = return [2, 3]

b' :: MaybeT IO [Int -> Int]
b' = return [(+ 3), (+ 4)]

-- Just [5,6,6,7]
re' :: MaybeT IO [Int]
re' = liftA2 (<*>) b' a'

--
--

-- https://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe
-- liftIO :: IO a -> MaybeT IO a
-- (m :: * -> *) a. MonadIO m => IO a -> m a
mgreet :: MaybeT IO ()
mgreet = do
  liftIO $ putStr "What is your name?" -- MaybeT IO ()
  n <- liftIO getLine -- MaybeT IO String
  liftIO $ putStrLn $ "Hello, " ++ n -- MaybeT IO ()

-- re0 = liftA2 (<*>) (Just (* 5)) (IO (Just 6) :: MaybeT IO Int)

re1 :: [Maybe Integer]
re1 = getZipList $ liftA2 (<*>) (ZipList [Just (* 5), Just (* 4)]) (ZipList [Just 6, Just 6])

-- lift := 普通のモナドを maybeT のようなモナド変換子にラップして返す
-- list m = MaybeT $ m >>= (\x -> return (Just x))

mt :: IO (Maybe ())
mt = runMaybeT $ (return 1 :: MaybeT IO Int) >>= lift . print -- print の返り値は unit () なため mt は `Just ()`

mt2 :: IO (Maybe Int)
mt2 =
  runMaybeT $
    (return 1 :: MaybeT IO Int) >>= \x -> do
      lift (print x) -- 計算途中の変数値を表示することができるが, impure なため避ける.
      return (x * 2)

-- Monad transform with MaybeT
type MaybeIO a = MaybeT IO a

-- runMaybeT getWord
getWordM :: MaybeIO String
getWordM = do
  lift (putStr "Input> ")
  a <- lift getLine
  when (a == "") (fail "")
  return a

testMTM01 :: MaybeIO String
testMTM01 = do
  a <- getWordM
  b <- getWordM
  return (a ++ b)

testMTM01' :: IO ()
testMTM01' = do
  a <- runMaybeT testMTM01
  case a of
    Nothing -> return ()
    Just s -> do putStrLn s; testMTM01'

-- 2 回まで空入力を受け入れる
testMTM02 :: MaybeIO String
testMTM02 = getWordM `mplus` getWordM `mplus` getWordM

-- Monad transform with ExceptT
type ExceptIO a = ExceptT String IO a

-- runExceptT getWordE
getWordE :: ExceptIO String
getWordE = do
  lift (putStr "Input> ")
  a <- lift getLine
  when (a == "") (throwError "empty string. ")
  return a

testMTE01 :: ExceptIO String
testMTE01 = do
  a <- getWordE
  b <- getWordE
  return (a ++ b)

testMTE01' :: IO ()
testMTE01' = do
  a <- runExceptT testMTE01
  case a of
    Left s -> putStrLn s
    Right s -> do putStrLn s; testMTE01'

testMTE02 :: ExceptIO String
testMTE02 = getWordE `mplus` getWordE `mplus` getWordE

-- catchError from Monad.Except -- print Error message, but still return `Right ""` as true value.
catched :: IO (Either String String)
catched = runExceptT $ (throwError "**Exception**: There is something wrong!" :: ExceptIO String) `catchError` \e -> do liftIO (print e); return ""

-- Right 1
mplusedEMonad :: IO (Either String Int)
mplusedEMonad = runExceptT $ (return 1 :: ExceptIO Int) `mplus` return 2

-- Left ""
mplusedEMonad' :: IO (Either String Int)
mplusedEMonad' = runExceptT $ (mzero :: ExceptT String IO Int) `mplus` mzero

-- Right "ok"
mplusedEMonad'' :: IO (Either String String)
mplusedEMonad'' = runExceptT $ (throwError "Error" :: ExceptT String IO String) `mplus` return "ok"