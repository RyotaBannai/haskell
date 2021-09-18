{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Archive.ArrowSyntax where

import Control.Monad
import Data.Char as C
import qualified Data.List.Extra as EL
import Data.Maybe
import qualified Data.Text as T

{-
Arrow Syntac | <https://kowainik.github.io/posts/arrows-zoo>
GHC | <https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/monad_comprehensions.html>
Data.List.Extra | <https://hackage.haskell.org/package/extra-1.7.10/docs/Data-List-Extra.html>
SOF | <https://stackoverflow.com/questions/6270324/in-haskell-how-do-you-trim-whitespace-from-the-beginning-and-end-of-a-string>
Type Synonym | <https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms>
-}

-- λ maybeAddEven (Just 2) (Just 2) # Just 4
maybeAddEven :: Maybe Int -> Maybe Int -> Maybe Int
maybeAddEven ma mb = case (ma, mb) of
  (Just a, Just b)
    | even a && even b -> Just (a + b)
    | otherwise -> Nothing
  (_, _) -> Nothing

-- Monad Comprehensions
maybeAddEven' :: Maybe Int -> Maybe Int -> Maybe Int
maybeAddEven' ma mb = [a + b | a <- ma, even a, b <- mb, even b]

-- Pattern guards
maybeAddEven'' :: Maybe Int -> Maybe Int -> Maybe Int
maybeAddEven'' ma mb
  | Just a <- ma,
    even a,
    Just b <- mb,
    even b =
    Just $ a + b
  | otherwise = Nothing

data User = User {userNickname :: T.Text, userName :: T.Text} deriving (Show)

-- View patterns
mkUser :: T.Text -> T.Text -> User
mkUser (T.toLower -> nickname) (T.toLower -> name) =
  User
    { userNickname = nickname,
      userName = name
    }

pairFromListWith :: (a1 -> a2) -> [a1] -> Maybe (a2, a2)
pairFromListWith f xs = case map f xs of
  [first, second] -> Just (first, second) -- [x,y] or x:y:[]
  _ -> Nothing

-- ?
-- pairFromListWith' f (x : map f -> xs) = Just (f x, head xs)
-- pairFromListWith' _ _ = Nothing

wrapMkUser :: [String] -> Maybe User
wrapMkUser = pairFromListWith T.pack >=> Just . uncurry mkUser -- `Maybe (Text, Text)` on the first part

test :: Maybe User
test = wrapMkUser ["rb", "ryotabannai"]

startWithA :: String -> Bool
startWithA (map C.toLower -> 'a' : _) = True
startWithA _ = False

{-
fromMaybe "" (Just "Hello, World!") -- "Hello, World!"
fromMaybe "" Nothing -- ""
-}
-- ViewPatterns の処理結果を変数 `isEven` へアサイン
maybeEven :: Maybe Int -> String
maybeEven (fromMaybe 1 -> even -> isEven) =
  if isEven
    then "Even number"
    else "Other"

maybeEven' :: Maybe Int -> String
maybeEven' (even . fromMaybe 1 -> isEven) =
  if isEven
    then "Even number"
    else "Other"

choose :: [String] -> IO ()
choose allowedStrs = do
  input <- getLine
  if
      | EL.trim input == "" -> putStrLn "Empty input" >> choose allowedStrs -- Or (T.strip . T.pack $ input)
      | map C.toLower input `elem` allowedStrs -> putStrLn $ "You choose:" <> input
      | otherwise -> putStrLn "Choose wisely" >> choose allowedStrs

-- Pattern synonyms
pattern Head :: a -> [a]
pattern Head x <- x : xs

-- λ foo [1..] # Just 1
foo :: [a] -> Maybe a
foo [] = Nothing
foo (Head x) = Just x