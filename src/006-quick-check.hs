import Test.QuickCheck

fun :: Integer -> Integer
fun x = 2 * x

myMax :: Integer -> Integer -> Integer
myMax x y = if x > y then x else y

prop_myMax x y = (myMax x y >= x)&&(myMax x y >= y)
                 &&(myMax x y == x || myMax x y == y)

prop_sqrt1 x = (sqrt x) ^ 2 == x

prop_sqrt2 x = abs((sqrt x)^2 - x) < 0.0001

prop_sqrt3 x = x>=0 ==> abs((sqrt x)^2 - x) < 0.0001

maxThree :: Float -> Float -> Float -> Float
maxThree x y z = if x >= y
                 then
                     (if x >= z then x else z)
                 else
                     (if y >= z then y else z)

prop_maxThree x y z = (maxThree x y z >= x)
                      &&(maxThree x y z >= y)
                      &&(maxThree x y z >= z)
                      &&(maxThree x y z == x
                         ||maxThree x y z == y
                         ||maxThree x y z == z) 