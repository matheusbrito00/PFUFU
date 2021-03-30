-- Exercicio 3-a
listInt :: Int -> Int -> [Int]
listInt a b 
    | a == b = [a]
    | a > b = []
    | otherwise = [a..b]

--Exercicio 3-b
listIntPares :: Int -> Int -> [Int]
listIntPares a b
    | a == b || a > b = []
    | (mod a 2) == 0 = [a,a+2..b]
    | otherwise = [a+1,a+3..b]
