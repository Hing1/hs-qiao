bmi1 :: Float -> Float -> Float
bmi1 h w = w / (h ^ 2)

bmi2 :: (Float, Float) -> Float
bmi2 (h, w) = w / (h ^ 2)


distance1 :: Float -> Float
distance1 x y = sqrt(x ^ 2 + y ^ 2)

distance2 :: (Float, Float) -> Float
distance2 (x, y) = sqrt(x ^ 2 + y ^ 2)

