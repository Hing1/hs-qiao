-- Toss two coins at a time
-- Conduct multiple tests
-- Count the probability that at least one coin is positive. (0 for positive)

import System.Random

-- experiment number_of_experiment number_of_toss
oneHeadPro :: Int -> Int -> IO Float
oneHeadPro m n = do
  xs <- rep (experiment n) m
  return (average xs)

-- experiment number_of_toss
experiment :: Int -> IO Float
experiment n = do
  xs <- rep (coinFlips 2) n
  return (oneHeadFre xs)

rep :: IO a -> Int -> IO [a]
rep x 0 = return []
rep x n = do
  y <- x
  ys <- rep x (n - 1)
  return (y:ys)

coinFlips :: Int -> IO [Int]
coinFlips 0 = return []
coinFlips n = do
  x <- coinFlip
  xs <- coinFlips (n - 1)
  return (x:xs)

coinFlip :: IO Int
coinFlip = randomRIO (0, 1)
    
oneHeadFre :: [[Int]] -> Float
oneHeadFre xs = fromIntegral (length [x | x <- xs, sum x <= 1]) / fromIntegral (length xs)

average :: [Float] -> Float
average xs = sum xs / fromIntegral (length xs)
