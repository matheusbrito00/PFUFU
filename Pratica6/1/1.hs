--1.a)
type Data = (Int, Int, Int)
valida :: Data -> Bool
valida (d, m, a)
    | d >= 1 && d <= 31 && (m == 1 || m==3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) && a >= 1900 && a <= 2100 = True
    | d >= 1 && d <= 30 && (m == 4 || m==6 || m == 9 || m == 11) && a >= 1900 && a <= 2100 = True
    | d >= 1 && d <= 28 && m == 2 && a >= 1900 && a <= 2100 = True
    | d == 29 && m == 2 && (bissexto a) && a >= 1900 && a <= 2100 = True
    | otherwise = False
    where
        bissexto x
            | mod x 4 == 0 && (mod x 100 /= 0 || mod x 400 == 0) = True
            | otherwise = False

-- 1.b)
bissextos :: [Int] -> [Int]
bissextos a = [ x | x <- a, bissexto x == True]
    where 
        bissexto x
            | mod x 4 == 0 && (mod x 100 /= 0 || mod x 400 == 0) = True
            | otherwise = False

-- 1.c)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados a b = [x | x <- a, verificaEmprestimo x b == True]
    where
        verificaEmprestimo (_, _, _, x, _) (d, m, a)
            | precede x (d, m, a) = True
            | otherwise = False
            where
                precede (d1, m1, a1) (d2, m2, a2)
                    | a1 < a2 = True
                    | a1 == a2 && m1 < m2 = True
                    | a1 == a2 && m1 == m2 && d1 < d2 = True
                    | otherwise = False

--1.d)
fibo2 :: Int -> Int
fibo2 n = fst(passo n)
    where
        passo n
            | n == 1 = (1, 1)
            | otherwise = par(passo (n-1))
            where 
                par (x, y) = (y, y+x)

--1.e)
fatorial :: Int -> Int 
fatorial n = prodIntervalo (1, n)
    where
        prodIntervalo (m, n) 
            | m > n = 0 
            | m == n = n
            | otherwise = m * prodIntervalo ((m+1), n)