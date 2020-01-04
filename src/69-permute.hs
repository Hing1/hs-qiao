module Solution where

-- Insert an element into a specified location in a list.
insert1 :: Int -> Int -> [Int] -> [Int]
insert1 x 0  ps= x:ps
insert1 x n (p:ps) = p:(insert1 x (n - 1) ps)

-- Insert an element at each possible location in a list.
insert2 :: Int -> [Int] -> [[Int]]
insert2 x ps = [insert1 x i ps | i <- [0,1.. (length ps)]]

perms :: Int  -> [[Int]]
perms 0 = [[0]]
perms n = concat [insert2 n p | p <- perms (n - 1)]