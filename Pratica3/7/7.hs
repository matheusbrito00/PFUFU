--A
prodIntervalo :: (Int, Int) -> Int
prodIntervalo (m, n) 
    | m > n = 0 -- Condição colocada para o programa não travar
    | m == n = n
    | otherwise = m * prodIntervalo ((m+1), n)

--B
fatorial :: Int -> Int 
fatorial n = prodIntervalo (1, n)