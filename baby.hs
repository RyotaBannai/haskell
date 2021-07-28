import Flow

-- ghci > :l baby
-- doubleMe 9

doubleMe x = x + x
doubleSmallNumber x = if x > 100 then x else x * 2  
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

-- odd is predicate only return when the number is
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
-- boomBangs [7..13]

removeNonUpperCase st = [c | c<-st, c `elem` ['A'..'Z']]
-- filter for nested list
xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
result = [[x | x <- xs, even x] | xs <- xxs]

-- right triangle that has integers for all sides and all sides equal to or smaller than 10 has a perimeter of 24
{-
This is a common pattern in functional programming. You take a starting set of solutions 
and then you apply transformations to those solutions and filter them until you get the right ones
-}
triangles = [(a,b,c) |c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2, a + b + c == 24]

triangles' = 
  let sameLen (a,b,c) = (a^2 + b^2 == c^2)
      totalEq (a,b,c) = a + b + c == 24
      f = filter sameLen .> filter totalEq
  in f [(a,b,c) |c <- [1..10], b <- [1..10], a <- [1..10]]

  