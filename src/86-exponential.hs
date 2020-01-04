module Exponential where

-- calculate x ^ n
exponential1 :: Float -> Int -> Float
exponential1 x 0 = x
exponential1 x n = x * exponential1 x (n - 1)

exponential2 :: Float -> Int -> Float
exponential2 x 0 = 1
exponential2 x 1 = x
exponential2 x 2 = x * x
exponential2 x n
  | mod n 2 == 0 = exponential2 y 2
  | mod n 2 == 1 = x * exponential2 y 2
  where
    m = div n 2
    y = exponential2 x m
