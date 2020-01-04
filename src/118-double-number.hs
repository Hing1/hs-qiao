action2 :: IO ()
action2 = do
  putStr "Input some integer : "
  n <- getLine
--  putStrLn ("Doubled is " ++ show (2 * (read n :: Int)))
  let x = read n :: Int
  putStrLn ("Doubled is " ++ show (2 * x))
