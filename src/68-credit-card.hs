module CreditCard where

-- Separate the digits of a number.
step1:: Integer -> [Integer]
step1 n
    |n < 10 = [n]
step1 n = n `mod` 10 : step1 (div n 10)

-- Double numbers with even subscript.
step2:: [Integer] -> [Integer]
step2 [] = []
step2 [x] = [x]
step2 (x:y:xs) = x : 2 * y : (step2 xs)

-- Add the digits of each number of the list.
step3:: [Integer] -> Integer
step3 [] = 0
step3 (x:xs)
    | x < 10 = x + step3 xs
    | otherwise = (mod x 10) + (div x 10) + (step3 xs)

-- Check whether the ID is legal.
validCard:: Integer -> Bool
validCard n = mod ((step3 . step2 . step1) n) 10 == 0
    