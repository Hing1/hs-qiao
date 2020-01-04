module Distance where

distance :: (Float, Float) -> Float
distance (x, y) = sqrt (x ^ 2 + y ^ 2)
