factors :: Int->[Int]
factors n = [x|x <- [1..n], n `mod` x == 0]

isPrime::Int->Bool
isPrime n = factors n == [1,n]