import System.Random

getInt :: IO Int
getInt = do
  x <- getLine
  return (read x :: Int)

guessIt :: Int -> IO ()
guessIt secret = do
  putStr "Please input your answer (1-100) : "
  guess <- getInt
  if (guess == secret) then
    putStrLn "You guessed it!"
  else if (guess < secret) then do
    putStrLn "Please guess a bigger number!"
    guessIt secret
  else if (guess > secret) then do
    putStrLn "Please guess a smaller number!"
    guessIt secret
  else
    return ()

guessNumber :: IO ()
guessNumber = do
  secret <- randomRIO (1, 100)
  guessIt secret
