module Fraction where

type Fraction = (Integer, Integer)

mygcd :: Integer -> Integer -> Integer
mygcd a b
  | a > b = mygcd (a - b) b
  | a < b = mygcd a (b - a)
  | otherwise = a

reduction :: Integer -> Integer -> Fraction
reduction x y
  |y == 0 = (0, 0)
  |x == 0 = (0, 1)
  |otherwise = (div x g, div y g)
  where g = mygcd x y

plusRat :: Fraction -> Fraction -> Fraction
plusRat (x1, y1) (x2, y2) = reduction (x1 * y2 + x2 * y1) (y1 * y2)

subtractRat :: Fraction -> Fraction -> Fraction
subtractRat (x1, y1) (x2, y2) = reduction (x1 * y2 - x2 * y1) (y1 * y2)

multiRat :: Fraction -> Fraction -> Fraction
multiRat (x1, y1) (x2, y2) = reduction (x1 * x2) (y1 * y2)

divRat :: Fraction -> Fraction -> Fraction
divRat (x1, y1) (x2, y2) = reduction (x1 * y2) (x2 * y1)

floorRat :: Fraction -> Integer
floorRat (x, y) = div x y

floatRat :: Fraction -> Float
floatRat (x, y) = fromInteger x / fromInteger y

eqRat :: Fraction -> Fraction -> Bool
eqRat (x1, y1) (x2, y2) = (x1 * y2 == x2 * y1)
