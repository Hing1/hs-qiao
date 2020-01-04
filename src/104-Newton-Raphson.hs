module Newton_Raphson where

--1. Function squareroot2
squareroot2 :: Float -> Integer -> Float
squareroot2 x 0
  |x /= 0 = x
squareroot2 x n
  |x /= 0 = ((squareroot2 x (n - 1)) + 2 / (squareroot2 x (n - 1))) / 2

--2. Function squareroot
squareroot :: Float -> Float -> Integer -> Float
squareroot r x 0
  |((r > 0)&&(x /= 0)) = x
squareroot r x n
  |((r > 0)&&(x /= 0)) = ((squareroot r x (n - 1)) + r / (squareroot r x (n - 1))) / 2

--3. Function sqrtSeq
sqrtSeq :: Float -> Float -> [Float]
sqrtSeq r x
  |((r > 0)&&(x /= 0)) = [squareroot r x n | n <- [0, 1..]]

--4. Function squareroot'
squareroot' :: Float -> Float -> Float -> Float
squareroot' r x epsilon
  |((r > 0)&&(x /= 0)&&(epsilon > 0)) = head [(squareroot r x n) | n <- [0, 1..], abs((squareroot r x n) - (squareroot r x (n + 1))) < epsilon] 

--5. Test Part

--5.1. For function squareroot2, there are three test cases
-- > squareroot2 1 2
-- 1.4166667
-- > squareroot2 2 2
-- 1.4166667
-- > squareroot2 5 2
-- 1.7203704

--5.2. For function squareroot, there are three test cases
-- > squareroot 2 1 0
-- 1.0
-- > squareroot 2 1 3
-- 1.4142157
-- > squareroot 2 1 5
-- 1.4142135

--5.3. For function sqrtSeq, there are two test cases
-- > take 10 (sqrtSeq 1 2)
-- [2.0, 1.25, 1.025, 1.0003049, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
-- > take 10 (sqrtSeq 2 1)
-- [1.0, 1.5, 1.4166667, 1.4142157, 1.4142135, 1.4142135, 1.4142135, 1.4142135, 1.4142135, 1.4142135]

--5.4. For function squareroot', there are five test cases
-- > squareroot' 2 1 0.1
-- 1.5
-- > squareroot' 2 1 0.01
-- 1.4166667
-- > squareroot' 2 1 0.001
-- 1.4142157
-- > squareroot' 2 1 0.0001
-- 1.4142157
-- > squareroot' 2 1 0.00001
-- 1.4142157
