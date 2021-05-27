-- Desenvolvido por
-- Matheus da Costa Brito
-- Vitor Santini Bessa

-- PARTE A 
-- Listas para os exercicios de ordenação 
l1=[1..2000]
l2=[2000,1999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]
teste = [1,2,4,3,5,7,6]

-- Ex 01 
--Original Bolha 
bolha :: (Ord a) => [a] -> ([a], Int)
bolha [] = ([], 0)
bolha lista = bolhaOrd lista 0 (length lista)

bolhaOrd lista i 0 = (lista, i)
bolhaOrd lista i n = bolhaOrd lst j (n - 1)
  where
    (lst, j) = troca (lista, i)

troca ([x], i) = ([x], i)
troca ((x:y:zs), i)
  | x > y = ((y:lst1), j1)
  | otherwise = ((x:lst2), j2)
    where
      (lst1, j1) = troca((x:zs),(i+1)) -- como inverte x com y soma 1 no contador
      (lst2, j2) = troca((y:zs),(i)) -- como não inverte x com y mantem o valor

-- Variação 1
bolha2 :: (Ord a) => [a] -> ([a], Int)
bolha2 [] = ([], 0)
bolha2 lista = bolhaOrd2 lista 0 (length lista)

bolhaOrd2 lista i n 
  | lista == lst = (lista, i) -- Se for igual a lista já está ordenada então faz a parada antecipada 
  | otherwise = bolhaOrd2 lst j (n - 1) -- Senão continua executando a ordenação
  where
    (lst, j) = troca2 (lista, i)

troca2 ([x], i) = ([x], i)
troca2 ((x:y:zs), i)
  | x > y = ((y:lst1), j1)
  | otherwise = ((x:lst2), j2)
    where
      (lst1, j1) = troca2((x:zs),(i+1)) -- como inverte x com y soma 1 no contador
      (lst2, j2) = troca2((y:zs),(i)) -- como não inverte x com y mantem o valor

-- Variação 2
bolha3 :: (Ord a) => [a] -> ([a], Int)
bolha3 [] = ([], 0)
bolha3 lista = bolhaOrd3 lista 0 (length lista)

bolhaOrd3 lista i n 
  | lista == lst = (lista, i) -- Se for igual a lista já está ordenada então faz a parada antecipada 
  | otherwise = (lst2 ++ [ult_elem], h)
  where
    ult_elem = last lst
    (lst, j) = troca3 (lista, i)
    lst1 = desconsidera_ult ult_elem lst
    (lst2, h) = bolhaOrd3 lst1 j (n - 1)

desconsidera_ult :: (Ord a) => a -> [a] -> [a]
desconsidera_ult _ [] = []
desconsidera_ult x (y:ys)
  | x == y = desconsidera_ult x ys
  | otherwise = y : desconsidera_ult x ys

troca3 ([x], i) = ([x], i)
troca3 ((x:y:zs), i)
  | x > y = ((y:lst1), j1)
  | otherwise = ((x:lst2), j2)
    where
      (lst1, j1) = troca3((x:zs),(i+1)) -- como inverte x com y soma 1 no contador
      (lst2, j2) = troca3((y:zs),(i)) -- como não inverte x com y mantem o valor

-- Ex 02
-- Original Seleção 
selecao :: (Ord a) => [a] -> ([a], Int)
selecao lst = selecao_aux lst 0

selecao_aux :: (Ord a) => [a] -> Int -> ([a], Int)
selecao_aux [] i = ([], i)
selecao_aux (x: xs) i = if x == y then (x: lst1, i1) else (y: lst2, i2)
  where 
    y = minimo (x: xs)
    (lst1, i1) = selecao_aux xs i
    (lst2, i2) = selecao_aux (remove y (x:xs)) (i + 1)

remove :: (Ord a) => a -> [a] -> [a]
remove a [] = []
remove a (x : xs)
  | a == x = xs
  | otherwise = x : remove a xs

minimo :: (Ord a) => [a] -> a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
  | x <= (minimo xs) = x
  | otherwise = minimo xs

-- Variação 1 
selecao2 :: (Ord a) => [a] -> ([a], Int)
selecao2 lst = selecao_aux2 lst 0

selecao_aux2 :: (Ord a) => [a] -> Int -> ([a], Int)
selecao_aux2 [] i = ([], i)
selecao_aux2 (x: xs) i = if x == y then (x: lst1, i1) else (y: lst2, i2)
  where 
    (lst_rest, y) = remove_menor (x: xs)
    (lst1, i1) = selecao_aux2 xs i
    (lst2, i2) = selecao_aux2 lst_rest (i + 1)

remove_menor [x] = ([], x)
remove_menor (x : xs : y)
  | x < xs = (xs : lst1, i1)
  | otherwise = (x : lst2, i2)
  where
    (lst1, i1) = remove_menor (x : y)
    (lst2, i2) = remove_menor (xs : y)

-- Variação 2
selecao3 :: (Ord a) => [a] -> ([a], Int)
selecao3 lst = selecao_aux3 lst 0

selecao_aux3 :: (Ord a) => [a] -> Int -> ([a], Int)
selecao_aux3 [] i = ([], i)
selecao_aux3 (x: xs) i = if x == y then (x: lst1, i1) else (y: lst2, i2)
  where 
    y = minimo3 (x: xs)
    (lst1, i1) = selecao_aux3 xs i
    (lst2, i2) = selecao_aux3 (remove3 y (x:xs)) (i + 1)

remove3 :: (Ord a) => a -> [a] -> [a]
remove3 a [] = []
remove3 a (x : xs)
  | a == x = xs
  | otherwise = x : remove3 a xs

minimo3 :: (Ord a) => [a] -> a
minimo3 = foldr1 min

-- Ex 03 
-- Original Inserção
insercao :: (Ord a) => [a] -> ([a], Int)
insercao [] = ([], 0)
insercao (x : xs) = (res, i1 + i2)
  where
    (lst, i1) = insercao xs
    (res, i2) = insereOrd x ((lst), 0)

insereOrd :: (Ord a) => a -> ([a], Int) -> ([a], Int)
insereOrd x ([], i) = ([x], i)
insereOrd x ((y : ys), i)
  | x <= y = (x : y : ys, i + 1)
  | otherwise = (y : lst, i1)
  where
    (lst, i1) = insereOrd x (ys, (i + 1)) 

-- Variação 1
insercao2 :: (Ord a) => [a] -> ([a], Int)
insercao2 = foldr insereOrd2 ([], 0)

insereOrd2 :: (Ord a) => a -> ([a], Int) -> ([a], Int)
insereOrd2 x ([], i) = ([x], i)
insereOrd2 x ((y : ys), i)
  | x <= y = (x : y : ys, i + 1)
  | otherwise = (y : lst, i1)
  where
    (lst, i1) = insereOrd2 x (ys, (i + 1)) 

-- Ex 04 
-- Quicksort Original 
quicksort :: (Ord a) => [a] -> ([a], Int)
quicksort [] = ([], 0)
quicksort (s : xs) = (menores ++ [s] ++ maiores, (length menores) + (length maiores) + i1 + i2)
  where
    (menores, i1) = quicksort [x | x <- xs, x < s]
    (maiores, i2) = quicksort [x | x <- xs, x >= s]

-- Variação 1
quicksort2 :: (Ord a) => [a] -> ([a], Int)
quicksort2 [] = ([], 0)
quicksort2 (s : xs) = (menores ++ [s] ++ maiores, (length maiores) + (length menores) + i1 + i2)
  where
    (menores, i1) = quicksort2 lst1
    (maiores, i2) = quicksort2 lst2
    (lst1, lst2) = divide s xs

divide n [] = ([], [])
divide n (x : xs)
  | n > x = (x : maiores, menores)
  | otherwise = (maiores, x : menores)
  where
    (maiores, menores) = divide n xs

-- Variação 2
quicksort3 :: (Ord a) => [a] -> ([a], Int)
quicksort3 [] = ([], 0)
quicksort3 [x] = ([x], 0)
quicksort3 [x, y] = if x >= y then ([y, x], 1) else ([x, y], 1)
quicksort3 lista =
  (menores ++ [w] ++ maiores, length maiores + length menores + i1 + i2 + n)
  where
    (w, n) = mediana (take 3 lista)
    (lst1, lst2) = divide3 w (remove w lista)
    (menores, i1) = quicksort3 lst1
    (maiores, i2) = quicksort3 lst2

divide3 n [] = ([], [])
divide3 n (x : xs)
  | n > x = (x : maiores, menores)
  | otherwise = (maiores, x : menores)
  where
    (maiores, menores) = divide3 n xs

mediana :: Ord a => [a] -> (a, Int)
mediana [x, y, z] = (mediana, i)
  where
    (rest, i) = quicksort [x, y, z]
    mediana = head (tail rest)

-- Ex 05
mergesort ::Ord a=>[a]->([a],Int)
mergesort [] = ([],0)
mergesort [x] = ([x],1)
mergesort xs = (e,b+d+f)
              where
                (ys,zs) = dividemerge xs
                (a,b) = mergesort ys
                (c,d) = mergesort zs
                (e,f) = merge a c

dividemerge ::Ord a=> [a] -> ([a],[a])
dividemerge [] = ([],[])
dividemerge [x] = ([x],[])
dividemerge (x:y:t)  = ((x:xs),(y:ys))
            where
                (xs,ys) = dividemerge t

merge :: Ord a=> [a]->[a]->([a],Int)
merge xs [] = (xs,1)
merge [] xs = (xs,1)
merge (x:xs) (y:ys)
 | x < y = (a,b+1)
 | otherwise = (c,d+1)
            where
                (a1,b) = merge xs (y:ys)
                a = x:a1
                (c1,d) = merge (x:xs) ys
                c = y:c1

------ PARTE B ------- 
-- Ex 06 
-- a
data Exp = 
    Val Int -- um numero
  | Add Exp Exp -- soma de duas expressoes 
  | Sub Exp Exp -- subtração de duas expressoes 
  | Mul Exp Exp -- Multiplica de duas expressoes 
  | Div Exp Exp -- Divide de duas expressoes 
  deriving (Eq, Ord, Show)

avalia :: Exp  -> Int
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2) 
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2)
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Div exp1 exp2) = (avalia exp1) `div` (avalia exp2)

--b
-- (3+12)*(15-5)/(1*3)
exp1 = Div (Mul (Add (Val 3) (Val 12)) (Sub (Val 15) (Val 5))) (Mul (Val 1) (Val 3))

-- -((6+8-5+1)*(2+6/2))
exp2 = Mul (Mul (Add (Add (Val 6) (Val 8)) (Add (Val (-5)) (Val 1))) (Add (Div (Val 6) (Val 2)) (Val 2))) (Val (-1))

-- Ex 07
--a
data Jogada =
   Pedra
  | Papel
  | Tesoura
  deriving (Eq, Ord, Show)

--b
vence :: Jogada -> Jogada -> Bool
vence x y 
  | x == (Pedra) && y == (Tesoura) = True
  | x == (Papel) && y == (Pedra) = True
  | x == (Tesoura) && y == (Papel) = True
  | otherwise = False

--c
vencedoras :: [(Jogada, Jogada)] -> [Jogada]
vencedoras [] = []
vencedoras (x: xs)
  | (fst x) == (snd x) || vence (fst x) (snd x) == True = [fst x] ++ vencedoras xs
  | otherwise = [snd x] ++ vencedoras xs

-- Ex 08
--a
data Nebuloso =
   Verdadeiro
  | Falso
  | Talvez Float
  deriving (Eq, Ord, Show)

--b
fuzzifica :: Float -> Nebuloso
fuzzifica x
  | x <= 0 = Falso
  | x >= 1 = Verdadeiro
  | otherwise = Talvez x

--c
verifica_alto :: Float -> (Nebuloso, Float)
verifica_alto x
  | fuzzifica y == Falso = (fuzzifica y, y)
  | fuzzifica y == Verdadeiro = (fuzzifica y, y)
  | otherwise = (fuzzifica y, y)
    where
      y = ((x - 1.7) / 0.2)

--d
verifica_barato :: Float -> (Nebuloso, Float)
verifica_barato x
  | fuzzifica y == Falso = (fuzzifica y, y)
  | fuzzifica y == Verdadeiro = (fuzzifica y, y)
  | otherwise = (fuzzifica y, y)
  where
    y = ((50.000 - x) / 20.00)

-- Ex 09
data Estudante =
   Colegios Ano NomeColegio MatriculaC Altura Peso
  | Universidades NomeUniversidade NomeCurso MatriculaU Altura Idade
  deriving (Eq, Ord, Show)

type Ano = Int
type NomeColegio = String
type MatriculaC = String
type Altura = Float
type Peso = Float
type NomeUniversidade = String
type NomeCurso = String
type MatriculaU = Int
type Idade = Int

type Estudantes = [Estudante]
bdEstudantes :: Estudantes
bdEstudantes = 
  [
    (Colegios 1 "IFTM" "QWERT" 1.60 50),
    (Colegios 1 "IFTM" "QWERT" 1.60 55),
    (Colegios 1 "IFTM" "QWERT" 1.60 60),
    (Colegios 2 "IFTM" "ASDFG" 1.80 65),
    (Colegios 2 "IFTM" "ASDFG" 1.80 70),
    (Colegios 2 "IFTM" "ASDFG" 1.80 75),
    (Colegios 3 "IFTM" "ASDFG" 1.80 80),
    (Colegios 3 "IFTM" "ASDFG" 1.80 85),
    (Colegios 3 "IFTM" "ZXCVB" 2.00 90),
    (Colegios 3 "IFTM" "ZXCVB" 2.00 95),
    (Universidades "UFU" "Computação" 12345678 1.65 18),
    (Universidades "UFU" "Computação" 12345678 1.65 19),
    (Universidades "UFU" "Computação" 12345678 1.65 20),
    (Universidades "UFU" "Computação" 87654321 1.88 21),
    (Universidades "UFU" "Computação" 87654321 1.88 22),
    (Universidades "UFU" "Computação" 87654321 1.88 23),
    (Universidades "UFU" "Computação" 87654321 1.88 24),
    (Universidades "UFU" "Computação" 87654321 1.88 25),
    (Universidades "UFU" "Computação" 87654321 1.94 26),
    (Universidades "UFU" "Computação" 87654321 1.94 27)
  ]

--Função para pegar apenas os dados dos segundo estudantes armazenados
getUniversidades :: Estudante -> Bool
getUniversidades (Universidades _ _ _ _ _) = True
getUniversidades (Colegios _ _ _ _ _) = False

descobre_altos :: Estudantes -> [(Int, Nebuloso)]
descobre_altos [] = []
descobre_altos (x: xs)
  | getUniversidades x == True = (getMat x, getNebuloso x): descobre_altos xs
  | otherwise = descobre_altos xs
    where
      getMat (Universidades _ _ m _ _) = m
      getNebuloso (Universidades _ _ _ a _) = fst (verifica_alto a)

-- Ex 10
data ArvBinInt = Nulo
                | No Int ArvBinInt ArvBinInt deriving (Show,Eq,Ord)

folhas :: ArvBinInt -> [Int]
folhas Nulo = []
folhas (No a no1 no2) = a:((folhas no1) ++ (folhas no2))                           -- pre-ordem, direita vem depois
folhas' Nulo = []
folhas' (No a (Nulo) no2) = (folhas' no2) ++ [a]
folhas' (No a (No a1 no11 no12) no2) = (folhas' no11) ++ (folhas' no12) ++ [a1] ++ (folhas' no2) ++ [a]

somaNosinternos :: ArvBinInt -> Int
somaNosinternos Nulo = 0
somaNosinternos (No a esq dir) = a + (somaNosinternos esq) + (somaNosinternos dir)

pertence :: Int -> ArvBinInt -> Bool
pertence _ Nulo = False
pertence x (No a esq dir) 
 |x == a = True
 |otherwise = (pertence x dir) || (pertence x esq)