-- 2.a)
bissexto :: Int -> Bool
bissexto x =
  let if1 = (mod x 400 == 0)
      if2 = (mod x 4 == 0)
      if3 = (mod x 100 /= 0)
   in if1 || (if2 && if3)

type Data = (Int, Int, Int)

valida :: Data -> Bool
valida (d, m, a) =
  let if1 = d >= 1 && d <= 31 && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12)
      if2 = d >= 1 && d <= 30 && (m == 4 || m == 6 || m == 9 || m == 11)
      if3 = d >= 1 && d <= 28 && m == 2 && not (bissexto a)
      if4 = d >= 1 && d <= 29 && m == 2 && (bissexto a)
   in if1 || if2 || if3 || if4

--2.b)
bissextos :: [Integer] -> [Integer]
bissextos a =
  let if1 = [x | x <- a, bissexto x]
   in if1

--2.c)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo :: Emprestimos
bdEmprestimo =
  [ ("H123C9", "BSI945", (12, 9, 2009), (20, 09, 2009), "aberto"),
    ("L433C5", "BCC021", (01, 9, 2009), (10, 09, 2009), "encerrado"),
    ("M654C3", "BCC008", (04, 9, 2009), (15, 09, 2009), "aberto")
  ]

precede :: Data -> Data -> Bool
precede (d1, m1, a1) (d2, m2, a2) =
  let if1 = not (valida (d1, m1, a1)) || not (valida (d2, m2, a2))
      if2 = a1 > a2
      if3 = a1 == a2 && m1 > m2
      if4 = a1 == a2 && m1 == m1 && d1 > d2
   in not (if1 || if2 || if3 || if4)

verificaEmprestimo :: Data -> Emprestimo -> Bool
verificaEmprestimo data (_, _, _, x, _) =
  let if1 = precede data x
   in if1

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados a b =
  let if1 = [x | x <- a, not (verificaEmprestimo b x)]
   in if1

--2.d)
passo :: (Integer, Integer) -> (Integer,Integer)
passo (x, y) =
  let if1 = (y, x + y)
   in if1

fibo2 :: Integer -> (Integer,Integer)
fibo2 0 = (0, 1)
fibo2 n =
  let if1 = passo (fibo2 (n -1))
   in if1

--2.e)
prodIntervalo :: Integer -> Integer -> Integer
prodIntervalo m n =
  let if1 =
        if (m >= n)
          then n
          else (m * (prodIntervalo (m + 1) n))
   in if1

fatorial :: Integer -> Integer
fatIorial n =
  let if1 = prodIntervalo 1 n
   in if1