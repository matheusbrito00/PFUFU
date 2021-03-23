--Guardas
fatorialG :: Int -> Int
fatorialG n
    | n == 0 = 1
    | otherwise = n * fatorialG (n-1)

--Casamento de Padrões
fatorialP :: Int -> Int
fatorialP 0 = 1
fatorialP n = n * fatorialP (n-1)