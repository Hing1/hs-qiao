module QuickSort where

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort (fst (partition x xs))) ++ [x] ++ (quickSort (snd (partition x xs)))

partition :: Ord a => a -> [a] -> ([a], [a])
partition x xs = (min, max)
  where min = filter (<=x) xs
        max = filter (>x) xs
