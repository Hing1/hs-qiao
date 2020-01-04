module MaxFunction where

maxFun :: (Integer -> Integer) -> Integer -> Integer
maxFun f 0 = f 0
maxFun f n = max (maxFun f (n - 1)) (f n)
