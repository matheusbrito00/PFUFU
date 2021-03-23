--Guardas
fiboG :: Int -> Int 
fiboG n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fiboG (n - 2) + fiboG (n-1)

--Casamento de Padrões 
fiboP :: Int -> Int
fiboP 0 = 0
fiboP 1 = 1
fiboP n = fiboP (n-2) + fiboP (n-1)