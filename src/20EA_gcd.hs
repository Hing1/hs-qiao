mygcd :: Integer -> Integer -> Integer
mygcd m n = if(m == 0) then n
  else if (n == 0) then m
  else if (m == n) then m
  else if (m > n) then if (m `mod` n == 0) then n else mygcd n (m `mod` n)
  else if (n `mod` m == 0) then m else mygcd m (n `mod` m)
