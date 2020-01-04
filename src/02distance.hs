distance :: Float -> Float -> Float
distance x y = sqrt(x ^ 2 + y ^ 2)

distance1 :: (Float, Float) -> Float
distance1 (x, y) = sqrt(x ^ 2 + y ^ 2)