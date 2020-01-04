factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

fib n
  | n == 0 = 1
  | n == 1 = 1
  | n > 1 = fib (n - 1) + fib(n - 2)

exponential :: Float -> Int -> Float
exponential x 0 = 1
exponential x 1 = x
exponential x n
  | mod n 2 == 0 = y ^ 2
  | mod n 2 == 1 = y ^ 2 * x
  where
  m = div n 2
  y = exponential x m
