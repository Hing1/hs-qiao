action1 :: IO()
action1 = do
  putStr "Input your name: "
  name <- getLine
  putStrLn ("\nHello " ++ name)

action2 :: IO()
action2 = do
  putStr "Input a integer: "
  n <- getLine
  let x = read n :: Int
  putStrLn ("\nDoubled is " ++ show (2 * x))

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
  putStr ("\nThe sum of " ++ show m ++ " and " show n ++ " is ")
  x <- getInt
  if (x == m + n)
    then putStr("Yes!\n")
    else putStr("No!\n")

repeatGetAnswer :: Int -> Int -> IO ()
repeatGetAnswer m n = do
  putStr (show m ++ " + " show n ++ " = ")
  x <- getInt
  if (x == m + n)
    then putStr("Yes!\n")
    else repeatGetAnswer m n

sumTwoNumbers :: IO ()
sumTwoNumbers = do
  (m, n) <- getTwoInts
  repeatGetAnswerm n



