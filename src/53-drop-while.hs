module DropWhile where

spaces :: String
spaces = " ,.;!"

dropSpaces :: String -> String
dropSpaces [] = []
dropSpaces (x:xs)
  | (elem x spaces) = dropSpaces xs
  | otherwise = x : xs

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile p [] = []
mydropWhile p (x:xs)
  | p x = mydropWhile p xs
  | otherwise = x:xs
