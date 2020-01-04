module Move where

data Move = Rock | Scissor | Paper deriving (Eq, Show)

beat :: Move -> Move -> Bool
beat Rock  Scissor = True
beat Scissor Paper = True
beat Paper    Rock = True
beat _           _ = False
