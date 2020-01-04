fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

sumFacs :: Integer -> Integer
sumFacs 0 = fac 0
sumFacs n = fac n + sumFacs (n - 1)

