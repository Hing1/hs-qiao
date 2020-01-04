module SumFunction where

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f 0 = f 0
sumFun f n = (f n) + (sumFun f (n - 1))