module Archive.ContTExpr where

import Control.Monad.Cont
import Control.Monad.Trans.Cont
import Data.Map
import Prelude hiding (lookup)

{-
Cont |<https://www.stackage.org/haddock/lts-11.15/mtl-2.2.2/Control-Monad-Cont.html#t:ContT>
ContT | <https://hackage.haskell.org/package/transformers-0.6.0.2/docs/Control-Monad-Trans-Cont.html#v:evalContT >
-}
textToSqlKey :: Map Int String
textToSqlKey =
  fromList
    [ (1, "SELECT * FROM %TABLE_NAME% WHERE %CONDS%"),
      (2, "DELETE FROM %TABLE_NAME% WHERE %CONDS%"),
      (3, "UPDATE %TABLE_NAME% SET %COLUMN%=%VALUE% WHERE %CONDS%")
    ]

users :: Map Int String
users = fromList [(1, "Jobs"), (2, "Mark")]

-- deleteTestR k id = do
--   let mParam = lookup textToSqlKey k

--   evalContT $ do
--     sql <- lookup textToSqlKey k !? "Query キーが見つかりませんでした。"
--     mRecord <- ContT $ (\s -> lookup users id)
--     deletedBrand <- mRecord !? "削除対象のデータが見つかりませんでした。"
--     ContT $ const "success"
--   where
--     Nothing !? e = ContT $ const e
--     Just a !? _ = ContT ($ a)

test :: Integer
test = evalCont (return 3)