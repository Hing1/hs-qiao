module Primes where

primes = filterPrime [2..]
  where filterPrime (p:xs) = 
          p : filterPrime [x | x <- xs, mod x p /= 0]
