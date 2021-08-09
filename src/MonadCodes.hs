module MonadCodes where

-- Just 3 `applyMaybe` \x -> Just (x+1) # Just 4
applyMaybe :: Maybe t -> (t -> Maybe a) -> Maybe a
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- Pierre's balance pole with birds
type Birds = Int

type Pole = (Birds, Birds)

-- landLeft 2 (landRight 1 (landLeft 1 (0,0))) -- enumate birds landing on the pole
landLeft :: Birds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

-- (0,0) -: landLeft 1 -: landRight 1 -: landLeft 2
(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

-- reworking
-- (0,0) -: landLeft' 10 # Nothing
-- return (0,0) >>= landLeft' 1 >>= landRight' 1 # Just (1,1)
-- Nothing >>= landLeft' 2 # `fail` will be propaged.
landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left, right)
  | abs ((left + n) - right) < 4 = Just $ landLeft n (left, right)
  | otherwise = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left, right)
  | abs (left - (right + n)) < 4 = Just $ landRight n (left, right)
  | otherwise = Nothing

-- return (0,0) >>= landLeft' 1 >>= landRight' 1 >>= banana >>= landRight' 1 # Nothing
banana :: Pole -> Maybe Pole
banana _ = Nothing