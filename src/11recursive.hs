mygcd :: Integer -> Integer -> Integer
mygcd x y 
  |x > y = mygcd (x - y) y
  |x < y = mygcd x (y - x)
  |x == y = x

fac :: Integer -> Integer
fac x
  | x == 0 = 1
  | x > 0 = x * fac(x - 1)

sumFacs :: Integer -> Integer
sumFacs 0 = fac 0
sumFacs x = sumFacs(x - 1) + fac x

f :: Int -> Int
f x = x * 2

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f 0 = f 0
sumFun f n = f n + sumFun f (n - 1)

maxFun :: (Integer -> Integer) -> Integer -> Integer
maxFun f 0 = f 0
maxFun f x = if f x > maxFun f (x - 1) then f x else maxFun f (x - 1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib(x - 1) + fib(x - 2)

sqrt2 :: Float -> Integer -> Float
sqrt2 x 0 = x
sqrt2 x n = (sqrt2 x (n - 1) + 2 / (sqrt2 x (n - 1))) / 2

findRoots :: Float -> Float-> Float -> (Float, Float)
findRoots a b c = ((-b + m) / (2 * a), (b + m) / (2 * a))
  where m = sqrt(b ^ 2 - 4 * a * c)
