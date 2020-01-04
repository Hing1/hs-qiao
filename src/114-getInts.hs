module Solution (
     getInts -- IO [Int]
         ) where
getInts :: IO [Int]
getInts = do
  s <- getLineInts
  return s

getLineInts :: IO [Int]
getLineInts = do
  line <- getLine
  if line == "" then return []
  else do
    s <- getLineInts
    return (stringToInt (getAllWords line) ++ s)

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