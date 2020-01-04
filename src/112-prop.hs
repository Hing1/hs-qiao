module Prop where

import Data.List

data Prop = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Imply Prop Prop
    deriving Eq

instance Show Prop where
  -- show :: Prop -> String
  show (Var c) = show c
  show (Not p) = "~" ++ show p
  show (And p1 p2) = "(" ++ show p1 ++ " && " ++ show p2 ++ ")"
  show (Or  p1 p2) = "(" ++ show p1 ++ " || " ++ show p2 ++ ")"
  show (Imply p1 p2)="(" ++ show p1 ++ " => " ++ show p2 ++ ")"

p1, p2, p3 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))
p2 = Or  (Var 'A') (Not (Var 'A'))
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

type Subst = [(Char, Bool)]

sub1 :: Subst
sub1 = [('A', True), ('B', False)]

eval :: Subst -> Prop -> Bool
eval sub (Var c) = head [b | (a, b) <- sub, a == c]
eval sub (Not p) = not (eval sub p)
eval sub (And p1 p2) = eval sub p1 && eval sub p2
eval sub (Or  p1 p2) = eval sub p1 || eval sub p2
eval sub (Imply p1 p2) = if (eval sub p1 == True && eval sub p2 == False) then False else True

vars :: Prop -> [Char]
vars (Var c) = c : []
vars (Not p) = vars p
vars (And p1 p2) = nub ((vars p1) ++ (vars p2))
vars (Or  p1 p2) = nub ((vars p1) ++ (vars p2))
vars (Imply p1 p2)=nub ((vars p1) ++ (vars p2))

substs :: Prop->[Subst]
substs p = varString (vars p)

varString::[Char] -> [Subst]
varString [] = []
varString [a] = [[(a, True)]] ++ [[(a, False)]]
varString (x:xs) = [(x, True ):a | a <- varString(xs)] 
                ++ [(x, False):a | a <- varString(xs)]

isTaut :: Prop -> Bool
isTaut p = length falseString == 0
  where falseString = [x | x <- [eval subst p | subst <- substs p], x == False]
