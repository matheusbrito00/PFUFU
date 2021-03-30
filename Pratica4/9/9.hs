sublistas :: [[Int]] -> [Int]
sublistas a = [x | y <- a , x <- y]