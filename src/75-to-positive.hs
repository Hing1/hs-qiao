module ToPositive where

toPositive :: Integer -> Integer
toPositive x = if x < 0 then (-2) * x - 1 else 2 * x
