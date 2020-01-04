module Empty where

-- [] and : is the constructor of list
isEmpty :: [Int] -> Bool
isEmpty [] = True
isEmpty (x:xs) = False
