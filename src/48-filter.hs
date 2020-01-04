f :: Integer -> Integer
f x = 2 * x

maxFun :: (Integer -> Integer) -> Integer -> Integer
maxFun f 0 = f 0
maxFun f n = max (maxFun f (n - 1)) (f n)

-- module Solution where
sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f 0 = f 0
sumFun f n = (f n) + (sumFun f (n - 1))


foo :: (Int -> Int) -> (Int -> Int)
foo f = f

boo :: (Int -> Int) -> Int -> (Int -> Int)
boo f 0 = f
boo f n = \x -> (boo f (n - 1)) (f x)

mymap f xs = [f x | x <- xs]

mymap :: (a -> b) -> [a] -> [b]

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p xs = [x | x <- xs, p x]

-- myfilter (\x -> mod x 2 == 0) [1, 2..10]