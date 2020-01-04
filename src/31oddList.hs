oddList :: Int -> [Int]
oddList n = [i | i <- [1..n], mod i 2 == 1]

