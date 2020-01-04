module InsertSort where

insertSort :: (a -> a -> Ordering) -> [a] -> [a]
insertSort cmp [] = []
insertSort cmp (x:xs) = insert cmp x (inserSort cmp xs)

insert :: (a -> a -> Ordering) -> a -> [a] -> [a]
insert cmp x [] = [x]
insert cmp x (y:ys) 
        |  (cmp x y) == LT = x:y:ys
        | otherwise = y : insert cmp x ys
