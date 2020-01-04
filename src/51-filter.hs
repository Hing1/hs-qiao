module Filter where

myfilter :: (a -> Bool) -> [a]-> [a]
myfilter p xs = [x | x <- xs, p x]
