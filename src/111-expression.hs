module Expression where

data Exp = Val Int | Var Char | Add Exp Exp | Sub Exp Exp | Mul Exp Exp

instance Show Exp where
  -- show :: Exp -> String
  show (Val x) = show x
  show (Var c) = show c
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

type Subst = [(Char, Int)]

sub1 :: Subst
sub1 = [('x', 2), ('y', 3)]

eval :: Exp -> Subst -> Int
eval (Val x) _ = x
eval (Var c) sub = head [b | (a, b) <- sub, a == c]
eval (Add e1 e2) sub = eval e1 sub + eval e2 sub
eval (Sub e1 e2) sub = eval e1 sub - eval e2 sub
eval (Mul e1 e2) sub = eval e1 sub * eval e2 sub

e1, e2, e3, e4, e5 :: Exp
e1 = Add (Val 1) (Val 2)
e2 = Mul e1 (Val 3)
e3 = Add e1 e2
e4 = Mul (Val 2) (Var 'x')
e5 = Add e4 (Var 'y')
