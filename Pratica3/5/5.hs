--Guardas
--n_tri :: Int -> Int
--n_tri n
--    | n == 0 = 0
--    | n == 1 = 1
--    | otherwise = n_tri (n-1) + n

--Casamento de Padrões
n_tri :: Int -> Int
n_tri 0 = 0
n_tri 1 = 1
n_tri n = n_tri (n-1) + n