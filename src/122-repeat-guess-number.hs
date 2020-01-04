import System.Random

getInt :: IO Int
getInt = do
  x <- getLine
  return (read x :: Int)

guessIt :: Int -> Int -> IO ()
guessIt secret current = do
  putStr "Please input your answer (1-100) : "
  guess <- getInt
  if (guess == secret) then
    putStrLn $ "Yes! You guessed it with " ++ show (current + 1) ++ " times"
  else if (guess < secret) then do
    putStrLn "Please guess a bigger number!"
    guessIt secret (current + 1)
  else if (guess > secret) then do
    putStrLn "Please guess a smaller number!"
    guessIt secret (current + 1)
  else
    return ()

guessNumber :: IO ()
guessNumber = do
  secret <- randomRIO (1, 100)
  guessIt2 secret 0