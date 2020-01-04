module Max where

mymax :: Ord a => a -> a -> a
mymax x y = if (x >= y) then x else y
