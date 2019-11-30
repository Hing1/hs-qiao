module Factors where

factors :: Int -> [Int]
factors n = [i | i <- [2..n], mod n i == 0]
