module Sum where

mySum :: Num t => [t] -> t
mySum [] = 0
mySum (x:xs) = x + mySum xs
