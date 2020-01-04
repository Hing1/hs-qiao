
primeFactors :: Int ->[Int]
primeFactors n = [x | x <- (factors n), myIsPrime x]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], mod n x == 0]

myIsPrime :: Int -> Bool
myIsPrime n = factors n == [1, n]


isIn :: Int -> [Int] -> Bool
isIn n [] = False
isIn n (x:xs) = if (n == x) then True else isIn n xs

isOrdered :: [Int] -> Bool
isOrdered [] = True
isOrdered [n] = True
isOrdered (a:(b:xs)) = if (a > b) then False else isOrdered (b:xs)