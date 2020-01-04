module DoubleElem where

doubleElem1 :: [Int] -> [Int]
doubleElem1 [] = []
doubleElem1 (x:xs) = 2 * x : doubleElem1 xs

doubleElem2 :: [Int] -> [Int]
doubleElem2 xs = [2 * x | x <- xs]
