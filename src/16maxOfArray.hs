mymax :: [Int] -> Int
mymax (x:xs) = if(length xs == 0) then x else (max x (mymax xs))

