module MySum where

mySum :: Int -> Int
mySum 1 = 1
mySum n = n + mySum (n - 1)
