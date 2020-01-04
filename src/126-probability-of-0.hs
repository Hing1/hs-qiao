-- Flip coins 
-- Find the probability of positive

module Simulation where
import System.Random

-- coinFlipPro number_of_experiment number_of_coinflips
coinFlipPro :: Int -> Int -> IO Float
coinFlipPro m n = do
  xs <- coinFlipPro1 m n
  return (average xs)

-- coinFlipPro1 number_of_experiment number_of_coinflips
coinFlipPro1 :: Int -> Int -> IO [Float]
coinFlipPro1 0 n = return []
coinFlipPro1 m n = do
  x <- coinFlipFre n
  xs <- coinFlipPro1 (m - 1) n
  return (x:xs)

coinFlipFre :: Int -> IO Float
coinFlipFre n = do
  xs <- coinFlips n
  return (headFre xs)

coinFlips :: Int -> IO [Int]
coinFlips 0 = return []
coinFlips n = do
  x <- coinFlip
  xs <- coinFlips (n - 1)
  return (x:xs)
    
-- coinFlip :: IO Char
-- coinFlip = do
--   x <- randomRIO (0, 1) :: IO Int
--   if (x == 0) then
--     return 'H'
--   else
--     return 'T'
coinFlip :: IO Int
coinFlip = randomRIO (0, 1)

headFre :: [Int] -> Float
headFre xs = fromIntegral (length [x | x <- xs, x == 0]) / fromIntegral (length xs)    

average :: [Float] -> Float
average xs = sum xs / fromIntegral (length xs)
