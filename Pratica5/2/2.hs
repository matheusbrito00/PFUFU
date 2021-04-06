npares :: [Int] -> Int
npares [] = 0
npares (x:xs)
    | mod x 2 == 0 = 1 + npares xs
    | otherwise = npares xs