module MyDay where

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving Eq

instance Show Day where
  -- (show) :: Day -> String
  show Sun = "Sunday"
  show Mon = "Monday"
  show Tue = "Tuesday"
  show Wed = "Wednesday"
  show Thu = "Thursday"
  show Fri = "Friday"
  show Sat = "Saturday"
{-
instance Eq Day where
  -- (==) :: Day -> Day -> Bool
  Sun == Sun = True
  Mon == Mon = True
  Tue == Tue = True
  Wed == Wed = True
  Thu == Thu = True
  Fri == Fri = True
  Sat == Sat = True
  _   == _   = False
-}
weekday :: Day -> Bool
weekday Sun = False
weekday Sar = False
weekday   _ = True

schedule :: Day -> String
schedule d = if d == Tue then "Haskell" else "Free"
