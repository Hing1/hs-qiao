myFactors :: Int -> [Int]
myFactors n = [i | i <- [2..n], mod n i == 0]

myIsPrime :: Int -> Bool
myIsPrime n = myFactors n == [n]

lessPrime :: Int -> [Int]
lessPrime n = [x | x <- [2..n], myIsPrime x]

