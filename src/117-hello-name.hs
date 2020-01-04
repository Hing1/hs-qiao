action1 :: IO ()
action1 = do
  putStr "Input your name : "
  name <- getLine
  putStr ("Hello " ++ name ++ " !\n")
  putStrLn ("Hello " ++ name ++ " !")
