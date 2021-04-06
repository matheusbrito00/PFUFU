comprime :: [[Int]] -> [Int]
comprime [] = []
comprime (x:xs) = x ++ comprime xs