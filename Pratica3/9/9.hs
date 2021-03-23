--Guardas 
mdcG :: (Int, Int) -> Int
mdcG (m,n)
    | n == 0 = m
    | otherwise = mdcG (n, (mod m n))

--Casamento de Padrões
mdcP :: (Int, Int) -> Int
mdcP (m, 0) = m
mdcP (m, n) = mdcP (n, (mod m n))