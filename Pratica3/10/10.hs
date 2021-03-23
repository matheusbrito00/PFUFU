--Guardas 
binoG :: (Int, Int) -> Int
binoG (n, k)
    | k == 0 = 1
    | k == n = 1
    | otherwise = binoG (n-1, k) + binoG (n-1, k-1)

--Casamento de Padrões 
binoP :: (Int, Int) -> Int
binoP (n, 0) = 1
binoP (n, k) = if (k == n)
    then 1
    else binoP (n-1, k) + binoP (n-1, k-1)