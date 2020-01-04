hasRealRoots :: Float -> Float -> Float -> Bool
hasRealRoots a b c = if(b ^ 2 - 4 * a * c > 0)  then True
                                                else False