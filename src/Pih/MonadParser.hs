{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- {-# LANGUAGE TupleSections #-}

module Pih.MonadParser where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Data.Char
import GHC.Base (Applicative)

-- import Data.Graph (Tree)

-- 使い切らなかった文字列も second element として返す.
-- 要素が一つの場合は`成功`、null の場合は`失敗`
-- parser が異なれば返したい`構文木`の種類も異なるため、Tree に限定せずに一般化する（型 Parser の型変数にする）
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
-- do 記法 := あるパーサーの出力文字列が次のパーサーの入力文字列となるように、パーサーを逐次的に`連接`する.
three' :: Parser (Char, Char)
three' = do
  x <- item
  item
  z <- item
  return (x, z)

-- 選択 := 複数のパーサーの組み合わせ方として、あるパーサーを入力文字列に適用し、もし失敗したら代わりに別のパーサーを同じ文字列に適用するという方法.
-- λ parse (item <|> return 'd') "abc"     # [('a',"bc")]
-- λ parse empty "abc"                     # []
-- λ parse (empty <|> item) "abc"          # [('a',"bc")]
-- λ parse (empty <|> empty <|>item) "abc" # [('a',"bc")]
instance Alternative Parser where
  -- empty :: Parser a
  empty = P (const [])

  -- (<|> :: Parser a -> Parser a -> Parser a)
  p <|> q =
    P
      ( \inp -> case parse p inp of
          [] -> parse q inp
          [(v, out)] -> [(v, out)]
      )

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

-- λ parse (char 'a') "abc"           # [('a',"bc")]
-- λ parse (upper <|> char 'a') "abc" # [('a',"bc")]
char :: Char -> Parser Char
char x = sat (== x)

-- string := 引数として与えた文字列と完全一致するかどうかチェック.
-- λ parse (string "abd") "abc" # []
-- λ parse (string "ab") "abc"  # [("ab","c")]
string :: String -> Parser String
string [] = return []
string (x : xs) = do
  char x
  string xs
  return (x : xs)

{-
Difference between `many` and `some`
λ parse (many digit) "ab" [("","ab")] -- 一つもなかった場合でも成功
λ parse (some digit) "ab" []          -- 一つでもない場合は失敗
-}
ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
nat = do
  xs <- some digit
  return (read xs)

-- 一つ以上の空白文字、タブ文字、改行文字が繰返される「空白」の parser
space :: Parser ()
space = do
  many (sat isSpace)
  return ()

-- λ parse int "-123 abc" # -123
int :: Parser Int
int =
  do
    char '-'
    n <- nat
    return (- n)
    <|> nat

-- 文法解析 := 文字解析　+ 構文解析.
-- トークン := 字句解析で生成されるデータの単位.

token :: Parser a -> Parser a
token p = do
  space
  v <- p
  space
  return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
  symbol "["
  n <- natural
  ns <-
    many
      ( do
          symbol ","
          natural -- nat は some なので一つ以上数値が無いと failure
      )
  symbol "]"
  return (n : ns)