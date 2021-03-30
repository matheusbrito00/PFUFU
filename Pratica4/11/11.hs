uniaoNRec :: [Int] -> [Int] -> [Int]
uniaoNRec a b = a ++ [ x | x <- b, elem x a == False]