module GetAllWord (getAllWords) where

getInts :: IO [Int]
getInts = do
  line <- getLine
  let a = stringToInt (getAllWords line)
  return a

stringToInt :: [String] -> [Int]
stringToInt [] = []
stringToInt (x:xs) = (read x :: Int):(stringToInt xs)

spaces :: String
spaces = [' ']

getWord :: String->String
getWord [] = []
getWord (x:xs) = 
    if(elem x spaces) then []
    else x:getWord xs

removeWord :: String->String
removeWord [] = []
removeWord (x:xs) =
    if(elem x spaces) then xs
    else removeWord xs

dropSpaces :: String -> String
dropSpaces [] = []
dropSpaces (x:xs)
    | (elem x spaces) = dropSpaces xs
    | otherwise = x : xs

getAllWords :: String->[String]
getAllWords [] = []
getAllWords x =
    if(ft == []) then getAllWords(dropSpaces(removeWord x))
    else ft:(getAllWords(dropSpaces(removeWord x)))
    where
        ft = getWord x