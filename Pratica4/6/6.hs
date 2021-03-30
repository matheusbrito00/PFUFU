seleciona_impares :: [Int] -> [Int]
seleciona_impares a = [ x | x <- a, mod x 2 == 1]