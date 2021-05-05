menores10 :: [Int] -> ([Int], Int)
menores10 lst = ([x | x <- lst, x < 10], length([x | x <- lst, x < 10]))