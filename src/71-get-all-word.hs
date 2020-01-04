module GetAllWord (getAllWords) where

spaces :: String
spaces = [' ', '\n','\t'] ++ ",./;'[]\\-=`<>?:{}|!@#$%^&*()_+\"1234567890"

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
    