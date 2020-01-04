module ThElem where

ithElem :: [t] -> Int -> t
ithElem (x:xs) 0 = x
ithElem (x:xs) n = ithElem xs (n - 1)
