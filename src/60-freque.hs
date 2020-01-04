module Solution (fre)  where

fre :: [String] -> [(String, Int)]
fre x = mergeSort myComp (frequency x)

myComp :: (String, Int) -> (String, Int) -> Ordering
myComp (s1, n1) (s2, n2)
  | n1 > n2 = LT
  | n1 < n2 = GT
  | otherwise = if (s1 > s2) then GT
                else if (s1 < s2) then LT
                else EQ

frequency :: [String] -> [(String, Int)]
frequency x = zip (unique xs) (lengths (unique xs) xs)
  where xs = toSmallString x

unique :: [String] -> [String]
unique [] = []
unique xs = (head xs) : (unique (filter ((/=) (head xs))xs))

lengths :: [String] -> [String] -> [Int]
lengths (x:ref) target = (length (filter (\a -> a == x) target)):(lengths ref target)

toSmallString :: [String] -> [String]
toSmallString [] = []
toSmallString (x:xs) = (toSmall x) : (toSmallString xs)

mergeSort ::   (a -> a -> Ordering) ->  [a] -> [a]
mergeSort cmp [] = []
mergeSort cmp [x] = [x]
mergeSort cmp xs = merge cmp (mergeSort cmp as) (mergeSort cmp bs)
  where 
    k = (length xs) `div` 2
    as = take k xs
    bs = drop k xs
            
merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge cmp [] b = b 
merge cmp a [] = a
merge cmp a@(x:xs) b@(y:ys) 
  | ((cmp x y) /= GT) = x:(merge cmp xs b)
  | otherwise = y:(merge cmp a ys)

toSmall :: String -> String
toSmall "" = ""
toSmall (xx:xs)
  | xx == 'Q' = 'q':toSmall xs
  | xx == 'W' = 'w':toSmall xs
  | xx == 'E' = 'e':toSmall xs
  | xx == 'R' = 'r':toSmall xs
  | xx == 'T' = 't':toSmall xs
  | xx == 'Y' = 'y':toSmall xs
  | xx == 'U' = 'u':toSmall xs
  | xx == 'I' = 'i':toSmall xs
  | xx == 'O' = 'o':toSmall xs
  | xx == 'P' = 'p':toSmall xs
  | xx == 'A' = 'a':toSmall xs
  | xx == 'S' = 's':toSmall xs
  | xx == 'D' = 'd':toSmall xs
  | xx == 'F' = 'f':toSmall xs
  | xx == 'G' = 'g':toSmall xs
  | xx == 'H' = 'h':toSmall xs
  | xx == 'J' = 'j':toSmall xs
  | xx == 'K' = 'k':toSmall xs
  | xx == 'L' = 'l':toSmall xs
  | xx == 'Z' = 'z':toSmall xs
  | xx == 'X' = 'x':toSmall xs
  | xx == 'C' = 'c':toSmall xs
  | xx == 'V' = 'v':toSmall xs
  | xx == 'B' = 'b':toSmall xs
  | xx == 'N' = 'n':toSmall xs
  | xx == 'M' = 'm':toSmall xs
  | otherwise = xx:toSmall xs
