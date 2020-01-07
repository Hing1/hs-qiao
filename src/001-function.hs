f :: Float -> Float
f x = x ^ 2

g :: Integer -> Integer
g x = if x < 0
        then (-2) * x - 1
        else 2 * x

h :: (Float, Float) -> Float
h (x, y) = sqrt(x ^ 2 + y ^ 2)

