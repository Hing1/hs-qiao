module Foldr where

{-
sum :: [Int] -> [Int]
sum [] = 0
sum (x:xs) = x + (sum xs)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && (and xs)

unlines :: [String] -> String
unlines [] = ""
unlines (x:xs) = x ++ "\n" ++ (unlines xs)

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ (concat xs)
-}
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f z [] = z
-- myfoldr f z (x:xs) = x `f` (myfoldr f z xs)
myfoldr f z (x:xs) = f x (myfoldr f z xs)

summ :: [Int] -> Int
summ = myfoldr (\x y -> x + y) 0

andd :: [Bool] -> Bool
andd = myfoldr (\x y -> x && y) True

unliness :: [String] -> String
unliness = myfoldr (\x y -> x ++ "\n" ++ y) ""

concatt :: [[a]] -> [a]
concatt = myfoldr (\x y -> x ++ y) []
