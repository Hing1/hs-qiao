module Tail where

mytail :: [t] -> [t]
mytail (x:xs) = xs
