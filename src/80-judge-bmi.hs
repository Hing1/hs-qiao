module JudgeBMI where

judgeBMI :: Float -> Float -> String
judgeBMI h m
  | k < 18.5  = "underweight"
  | k <= 24   = "normal"
  | otherwise = "overweight"
  where
    k = m / h ^ 2
