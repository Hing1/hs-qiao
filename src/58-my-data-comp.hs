data MyOrdering = LT | EQ | GT deriving (Show, Eq)

mycomp :: Int -> Int -> MyOrdering
mycomp x y
  | x > y = LT
  | x == y = EQ
  | x < y = GT