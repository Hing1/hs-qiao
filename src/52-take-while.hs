module TakeWhile where

isAlpha :: Char -> Bool
isAlpha x = elem x "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm"

getWords :: String -> String
getWords [] = []
getWords (x:xs)
  | isAlpha x = x : (getWords xs)
  | otherwise = []

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile p [] = []
mytakeWhile p (x:xs)
  |p x = x : (mytakeWhile p xs)
  |otherwise = []
