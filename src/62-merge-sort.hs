module MergeSort where

mergeSort ::   (a -> a -> Ordering) ->  [a] -> [a]
mergeSort cmp [] = []
mergeSort cmp [x] = [x]
mergeSort cmp xs = merge cmp (mergeSort cmp head) (mergeSort cmp tail)
  where 
    mid = (length xs) `div` 2
    head = take mid xs
    tail = drop mid xs
            
merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge cmp [] b = b 
merge cmp a [] = a
merge cmp a@(x:xs) b@(y:ys) 
  | ((cmp x y) /= GT) = x:(merge cmp xs b)
  | otherwise = y:(merge cmp a ys)
