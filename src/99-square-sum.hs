module SquareSum where

squareSum1 :: Int -> Int
squareSum1 1 = 1
squareSum1 n = n ^ 2 + squareSum1 (n - 1)

squareSum2 :: Int -> Int
squareSum2 n = sum [x ^ 2 | x <- [1..n]]
