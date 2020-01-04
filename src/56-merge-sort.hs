module MergeSort where

-- merge :: Ord a => [a] -> [a] -> [a]
-- merge xs [] = xs
-- merge [] ys = ys
-- merge (x:xs) (y:ys)
--         | x > y = y:merge (x:xs) ys
--         | otherwise = x:merge xs (y:ys)
        
-- mergeSort :: Ord a => [a] -> [a]
-- mergeSort xs = merge (mergeSort x1) (mergeSort x2)
--         where
--             (x1, x2) = split xs
--             split xs = (take mid xs, drop mid xs)
--             mid = (length xs) `div` 2

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort head) (mergeSort tail)
  where 
    mid = (length xs) `div` 2
    head = take mid xs
    tail = drop mid xs
            
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] b = b 
merge a [] = a
merge a@(x:xs) b@(y:ys) 
  | x <= y    = x:(merge xs b)
  | otherwise = y:(merge a ys)
