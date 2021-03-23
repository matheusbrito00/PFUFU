par :: (Int, Int) -> (Int, Int)
par (x, y) = (y, y+x)

passo :: Int ->  (Int, Int)
passo n
    | n == 1 = (1, 1)
    | otherwise = par(passo (n-1))

fibo2 :: Int -> Int
fibo2 n = fst(passo n)