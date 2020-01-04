module Lambda where

f :: Int -> Int
f x = 2 * x
ff :: Int -> Int
ff = \x -> (2 * x)

g :: Int -> Int
g x = x + 2
gg :: Int -> Int
gg = \x -> x + 2

k :: Int -> Int -> Int
k = (\x y -> x + y)

l :: (Int, Int) -> Int
l = (\(x, y) -> (2 * x + 2 * y))
