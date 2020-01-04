getInt :: IO Int
getInt = do
  x <- getLine
  return (read x :: Int)

getTwoInts :: IO (Int, Int)
getTwoInts = do
  m <- getInt
  n <- getInt
  return (m, n)

addTwoNumbers :: IO ()
addTwoNumbers = do
  (m, n) <- getTwoInts
  putStr ("The sum of " ++ show m ++ " and " ++ show n ++ " is ")
  x <- getInt
  if (x == m + n)
    then putStrLn "Good!"
    else putStrLn "No!"

