getInt :: IO Int
getInt = do
  x <- getLine
  return (read x :: Int)

exer2 :: IO [Int]
exer2 = do
  n <- getInt
  if (n < 0) then
    return []
  else do
    ns <- exer2
    return (n:ns)

exer3 :: IO Int
exer3 = do
  xs <- exer2
  return (sum xs)