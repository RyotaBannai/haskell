-- ghci > :l baby
-- doubleMe 9

doubleMe x = x + x
doubleSmallNumber x = if x > 100 then x else x * 2  
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1