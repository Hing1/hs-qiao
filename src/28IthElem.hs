myIthElem :: [t] -> Int -> t
myIthElem (x:xs) 0 = x
myIthElem (x:xs) n = myIthElem xs (n - 1)

