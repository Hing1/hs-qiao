mygcd :: Integer -> Integer -> Integer
mygcd x y
  | (x > y)   = mygcd (x - y) y
  | (x < y)   = mygcd x (y - x)
  | otherwise = x

-- **********************************

factorial :: Integer -> Integer
factorial x
  | (x == 0) = 1
  | (x > 0)  = x * factorial (x - 1)

sum_factorial :: Integer -> Integer
sum_factorial 0 = factorial 0
sum_factorial x = factorial x + sum_factorial (x - 1)

-- **********************************

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

-- **********************************

exponential :: Float -> Int -> Float
exponential x 0 = 1
exponential x 1 = x
exponential x n
  | mod n 2 == 0 = y ^ 2
  | mod n 2 == 1 = y ^ 2 * x
  where
  m = div n 2
  y = exponential x m
