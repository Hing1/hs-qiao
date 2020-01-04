mySum :: [Num] -> Num 
mySum [] = 0
mySum (x:xs) = x + mySum xs

