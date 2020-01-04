module BMI where

bmi :: (Float, Float) -> Float
bmi (h, m) = m / h ^ 2

bmi_t :: Float -> Float -> Float
bmi_t h m = m / h ^ 2
