module People where

data People = Person String Int

instance Show People where
  show (Person name age) = "Name: " ++ name ++ ", Age: " ++ (show age)
