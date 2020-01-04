insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert n (x:xs) = if (n < x) then n:(x:xs) else x:(insert n xs)

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)