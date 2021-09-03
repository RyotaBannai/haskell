{-# LANGUAGE LambdaCase #-}

-- {-# LANGUAGE TupleSections #-}

module Pih.MonadParser where

import Control.Applicative
import Data.Char
import GHC.Base (Applicative)

-- import Data.Graph (Tree)
-- 使い切らなかった文字列も second element として返す.
-- 要素が一つの場合は`成功`、null の場合は`失敗`
-- parser が異なれば返したい構文技の種類も異なるため、Tree に限定せずに一般化する（型 Parser の型変数にする）
-- 型 a の parser は関数であり、その型は String を受け取り [] を返す型.
newtype Parser a = P (String -> [(a, String)])

-- StateMonad の app と同じ.
parse :: Parser a -> String -> [(a, String)]
parse (P p) = p

-- Simple form of `Parser`
-- λ parse item ""     # []
-- λ parse item "abcd" # [('a',"bcd")]
item :: Parser Char
item =
  P
    ( \case
        [] -> []
        (x : xs) -> [(x, xs)]
    )

-- λ parse (fmap toUpper item) "abcd" # [('A',"bcd")]
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> [(g v, out)]
      )

--  λ parse (pure 1) "abcd" # [(1,"abcd")] -- 入力を消費せずに必ず成功する.
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v, inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px =
    P
      ( \inp -> case parse pg inp of
          [] -> []
          [(g, out)] -> parse (fmap g px) out
      )

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    P
      ( \inp -> case parse p inp of
          [] -> []
          [(v, out)] -> parse (f v) out
      )

-- λ parse three "abcdefg" # [(('a','c'),"defg")]
-- λ parse three "ab"      # []
three :: Parser (Char, Char)
three = g <$> item <*> item <*> item
  where
    g x y z = (x, z)

{-
Nitty-gritties:
`<*>` では fmap を適用する. これにより、px（Parser）でパースした結果を g に apply する関数（状態変換器）が新たに構築され、その関数を out と一緒に parse にかける. (parse は状態変換器に入力値を適用する)
g は複数の引数を持ちうるため、three のような場合だと parse にかけた時、部分適用（初めの引数 x が埋まる）となった状態で、tuple の左側の要素として返される. つまり [(g y z = ('a',z),"bcd")] が結果となり、`<*>` で連接した次のパーサの `inp` となる.
-}

-- With Monad.
three' :: Parser (Char, Char)
three' = do
  x <- item
  item
  z <- item
  return (x, z)