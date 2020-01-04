module Solution where
-- import Hand
data Hand = Rock | Scissor | Paper deriving ( Eq, Enum)

playOneHand :: Hand -> Hand -> (Int, Int)
playOneHand x y
  | x == y = (0, 0)
  | x == Rock = if (y == Scissor) then (1, 0) else (0, 1)
  | x == Scissor = if (y == Paper) then (1, 0) else (0, 1)
  | x == Paper = if (y == Rock) then (1, 0) else (0, 1)

playManyHands :: [Hand] -> [Hand] -> (Int, Int)
playManyHands [] [] = (0, 0)
playManyHands (x:xs) (y:ys) = (x1 + x2, y1 + y2)
  where x1 = fst (playOneHand x y)
        y1 = snd (playOneHand x y)
        x2 = fst (playManyHands xs ys)
        y2 = snd (playManyHands xs ys) 