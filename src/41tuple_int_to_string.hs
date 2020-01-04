map2stringtuple :: [(String, Int, Int)] -> [(String, String, String)]
map2stringtuple [] = []
map2stringtuple ((s, m, n):xs) = [(s, (show m), (show n))] ++ (map2stringtuple xs)