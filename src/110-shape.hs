module Shape where

data Shape = Circle Float | Rectangle Float Float deriving (Show, Eq)

c :: Shape
c = Circle 2.3

r :: Shape
r = Rectangle 1 2.3

isRound :: Shape -> Bool
isRound (Circle c) = True
isRound _          = False

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle l w) = l * w

circum :: Shape -> Float
circum (Circle r) = 2 * pi * r
circum (Rect a b) = 2 * a + 2 * b
