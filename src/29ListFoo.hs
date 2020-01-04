foo1 :: [Int] -> [Int]
foo1 [] = []
foo1 (x:xs) = 3 * x + 1 : foo xs

foo2 :: [Int] -> [Int]
foo2 xs = [3 * x + 1 | x <- xs]

