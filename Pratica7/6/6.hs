solucoes :: [Int] -> [Int]
solucoes l = filter (\x -> (5 * x + 6) < (x * x)) l