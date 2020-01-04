module FuncCompost where

takeWord :: String -> String
-- takeWord = getWords (dropSpaces xs)
takeWord = getWords . dropSpaces

isAlpha :: Char -> Bool
isAlpha x = elem x "QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm"

getWords :: String -> String
getWords [] = []
getWords (x:xs)
  | isAlpha x = x : (getWords xs)
  | otherwise = []

spaces :: String
spaces = " ,.;!"

dropSpaces :: String -> String
dropSpaces [] = []
dropSpaces (x:xs)
  | (elem x spaces) = dropSpaces xs
  | otherwise = x : xs

