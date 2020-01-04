module SelectSort where

selectionSort ::  Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = (fst (selectMin xs)) : (selectionSort (snd (selectMin xs)))

selectMin :: Ord a => [a] -> (a, [a])
selectMin x = (min, xs)
  where min = minimum x
        xs = erase min x

erase :: Ord a => a -> [a] -> [a]
erase x [] = []
erase x (m:xs) = if (m == x) then xs else m:(erase x xs)
