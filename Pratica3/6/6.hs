--Guardas
--potencia2 :: Int -> Int
--potencia2 n
--    | n == 0 = 1
--    | otherwise = potencia2 (n-1) * 2

--Casamento de Padrões
potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 n = potencia2 (n-1) * 2