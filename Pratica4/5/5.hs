quadrados :: Int -> Int -> [Int]
quadrados a b = [x*x | x <- [a..b], a < b]