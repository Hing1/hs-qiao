module SqrtCheck where
import Test.QuickCheck


prop_sqrt x = (x >= 0) ==> abs ((sqrt x) ^ 2 - x) < 0.0001
