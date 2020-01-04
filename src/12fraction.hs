type Fraction = (Integer, Integer)

normalize :: Fraction -> Fraction
normalize (x, y) 
  | x * y < 0 = (-div ax g, div ay g)
  | x * y > 0 = (div ax g, div ay g) 
  | x == 0 = (0, 1)
   where ax = abs x
         ay = abs y
         g = gcd ax ay 

ratplus :: Fraction -> Fraction -> Fraction
ratplus (x1, y1) (x2, y2) = normalize (x1 * y2 + x2 * y1, y1 * y2)

