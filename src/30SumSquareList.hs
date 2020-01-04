squareList1 :: Int -> Int
squareList1 1 = 1
squareList1 n = n ^ 2 + squareList1 (n - 1)

squareList2 :: Int -> Int
squareList2 n = sum [x ^ 2 | x <- [1..n]]

