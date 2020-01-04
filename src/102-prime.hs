module Prime where

factors :: Int -> [Int]
factors n = [i | i <- [2..n], mod n i == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [n]
