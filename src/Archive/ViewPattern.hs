{-# LANGUAGE ViewPatterns #-}

module Archive.ViewPattern where

import Control.Monad
import Data.Char as C
import Data.Maybe
import Data.Text as T (Text, pack, toLower)

maybeAddEven :: Maybe Int -> Maybe Int -> Maybe Int
maybeAddEven ma mb = case (ma, mb) of
  (Just a, Just b)
    | even a && even b -> Just (a + b)
    | otherwise -> Nothing
  (_, _) -> Nothing

data User = User {userNickname :: Text, userName :: Text} deriving (Show)

-- ViewPatterns
mkUser :: Text -> Text -> User
mkUser (T.toLower -> nickname) (T.toLower -> name) =
  User
    { userNickname = nickname,
      userName = name
    }

pairFromListWith :: (a1 -> a2) -> [a1] -> Maybe (a2, a2)
pairFromListWith f xs = case map f xs of
  [first, second] -> Just (first, second) -- [x,y] or x:y:[]
  _ -> Nothing

-- Maybe (Text, Text)
wrapMkUser :: [String] -> Maybe User
wrapMkUser = pairFromListWith T.pack >=> Just . uncurry mkUser

test :: Maybe User
test = wrapMkUser ["rb", "ryotabannai"]

startWithA :: String -> Bool
startWithA (map C.toLower -> 'a' : _) = True
startWithA _ = False

{-
fromMaybe "" (Just "Hello, World!") -- "Hello, World!"
fromMaybe "" Nothing -- ""
-}
-- isEven をボディで変数として利用
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