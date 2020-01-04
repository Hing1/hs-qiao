rep :: IO a -> Int -> IO [a]
rep fun 0 = return []
rep fun n = do
  y <- fun
  ys <- rep fun (n - 1)
  return (y:ys)

getInt :: IO Int
getInt = do
  x <- getLine
  return (read x :: Int)

exer1 :: IO [Int]
exer1 = do
  putStr "Please input a number : "
  n <- getInt
  putStrLn $ "Please input " ++ show n ++ " numbers"
  xs <- rep getInt n
  return xs
