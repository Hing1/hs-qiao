module PrimeList where

factors :: Int -> [Int]
factors n = [i | i <- [2..n], mod n i == 0]

isPrime :: Int -> Bool
isPrime n = factors n == [n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], isPrime x]
