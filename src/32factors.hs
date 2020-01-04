myFactors :: Int -> [Int]
myFactors n = [i | i <- [2..n], mod n i == 0]

