tuplelist2string :: [(String, String, String)] ->String
tuplelist2string [] = ""
tuplelist2string ((s1, s2, s3):xs) = s1 ++ " " ++ s2 ++ " " ++ s3 ++ "\n" ++ (tuplelist2string xs)