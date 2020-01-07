f :: Integer -> Integer
f x = if (x >= 0)
        then x * 2
        else -(2 * x + 1)

hasRealRoot :: Float -> Float -> Float -> Bool
hasRealRoot a b c = if (b ^ 2 - 4 * a * c >= 0)
                      then True
                      else False

isEven :: Integer -> Bool
isEven x = if (x `mod` 2 == 0)
             then True
             else False

