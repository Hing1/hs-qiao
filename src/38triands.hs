triands :: Integer -> [(Integer, Integer, Integer)]
triands n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a ^ 2 + b ^ 2 == c ^ 2]
