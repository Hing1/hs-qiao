module Max where
import Test.QuickCheck

mymax :: Integer -> Integer -> Integer
mymax x y = if x > y then x else y

prop_mymax x y = (mymax x y >= x)
              && (mymax x y >= y)
              && ((mymax x y == x) || (mymax x y == y))
