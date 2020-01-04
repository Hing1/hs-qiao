module Map where

mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = (f x) : (mymap f xs)
