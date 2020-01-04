getInt :: IO Int
getInt = do
  x <- getLine
  return (read x :: Int)

getTwoInts :: IO (Int, Int)
getTwoInts = do
  m <- getInt
  n <- getInt
  return (m, n)

repeatGetAnswer :: Int -> Int -> IO ()
repeatGetAnswer m n = do
  putStr ("The sum of " ++ show m ++ " and " ++ show n ++ " is ")
  x <- getInt
  if (x == m + n)
    then putStrLn "Good!"
    else repeatGetAnswer m n

sumTwoNumbers :: IO ()
sumTwoNumbers = do
  (m, n) <- getTwoInts
  repeatGetAnswer m n