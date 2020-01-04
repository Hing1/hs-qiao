module Odd where

oddlist :: Int -> [Int]
oddlist 1 = [1]
oddlist n = [i | i <- [1..n], mod i 2 == 1]
