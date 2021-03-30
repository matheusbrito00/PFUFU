type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede :: Data -> Data -> Bool
precede (d1, m1, a1) (d2, m2, a2)
    | a1 < a2 = True
    | a1 == a2 && m1 < m2 = True
    | a1 == a2 && m1 == m2 && d1 < d2 = True
    | otherwise = False

verificaEmprestimo :: Emprestimo -> Data -> Bool
verificaEmprestimo (_, _, _, x, _) (d, m, a)
    | precede x (d, m, a) = True
    | otherwise = False

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados a b = [x | x <- a, verificaEmprestimo x b == True]