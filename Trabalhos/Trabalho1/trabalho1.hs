--Questão 1 
analisa_raizes :: Int -> Int -> Int -> String
analisa_raizes a b c 
    | a == 0 = "Equacao degenerada"
    | b * 2 > 4 * a * c = "Possui duas raizes reais"
    | b * 2 == 4 * a * c = "Possui uma raiz real"
    | b * 2 < 4 * a * c = "Nenhuma raiz real"

--Questão 2
equacao :: Float -> Float -> Float -> (Float, Float)
equacao a b c
    | a == 0 = ((-c)/b, a)
    | otherwise = (((-b) + (sqrt(b^2 - 4*a*c)))/2*a, ((-b) - (sqrt(b^2 - 4*a*c)))/2*a)

--Questão 3
type Data = (Int, Int, Int)

idade :: Data -> Data -> Int
idade (da, ma, aa) (dn, mn, an)
    | aa == an = 0
    | ma < mn  = (aa - an) - 1
    | ma == mn && da < dn = (aa - an) - 1
    | otherwise = aa - an

passagem :: Float -> Data -> Data -> Float
passagem vt da dn 
    | idade da dn < 2 = (15/100) * vt
    | idade da dn > 2 && idade da dn < 10 = (40/100) * vt
    | idade da dn >= 70 = (50/100) * vt
    | otherwise = vt

--Questão 4
gera1 :: [Int]
gera1 = [ x^3 | x <- [1..20], x > 3, x < 11, even x]
-- [64,216,512,1000]
gera2 :: [(Int, Int)]
gera2 = [ (x, y) | x <- [1..20], y <- [1..20], x <= 5, y >= x && y <= x*3]
--[(1,1),(1,2),(1,3),(2,2),(2,3),(2,4),(2,5),(2,6),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9),(4,10),(4,11),(4,12),(5,5),(5,6),(5,7),(5,8),(5,9),(5,10),(5,11),(5,12),(5,13),(5,14),(5,15)]
l1 = [15,16]
gera3 :: [Int]
gera3 = [ x | y <- l1, x <- [1..y]]
--[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
gera4 :: [(Int, Int)]
gera4 = [ (x, y) | x <- [1..10], y <- [x+1], even x]
--[(2,3),(4,5),(6,7),(8,9),(10,11)]
gera5 :: [Int]
gera5 = [ x + y | (x, y) <- gera4]
--[5,9,13,17,21]

--Questão 5
--a
contaNegM2 :: [Int] -> Int
contaNegM2 l1 = length [ x | x <- l1, x > 0 && mod x 3 == 0]
--b
listaNegM2 :: [Int] -> [Int]
listaNegM2 l1 = [ x | x <- l1, x > 0 && mod x 3 == 0]

--Questão 6 
fatores :: Int -> [Int]
fatores n = [ i | i <- [1..n], (mod n i) == 0]

primos :: Int -> Int -> [Int]
primos x y = [ n | n <- [x..y], (fatores n) == [1,n]]

--Questão 7
mmc :: Int -> Int -> Int -> Int 
mmc a b c 
    | a == b && b == c = a
    | otherwise = (head [n | n <- [1..], (mod n a) == 0 && (mod n b) == 0 && (mod n c) == 0])

--Questão 8 
serie :: Float -> Int -> Float 
serie x 1 = 1 / x
serie x n 
    | (even n) = x / (fromIntegral n) + (serie x (n - 1))
    | (odd n) = (fromIntegral n) / x + (serie x (n - 1))

--Questão 9
fizzbuzz :: Int -> [String]
fizzbuzz 1 = ["No"]
fizzbuzz n 
    | (mod n 2) == 0 && (mod n 3) == 0 = (fizzbuzz (n-1))++["FizzBuzz"]
    | (mod n 2) == 0 = (fizzbuzz (n-1))++["Fizz"]
    | (mod n 3) == 0 = (fizzbuzz (n-1))++["Buzz"]
    | otherwise = (fizzbuzz (n-1))++["No"]

--Questão 10
seleciona_multiplos :: Int -> [Int] -> [Int]
seleciona_multiplos n l = [ x | x <- l, (mod x n) == 0]

--Questão 11
unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia _ [] = False
unica_ocorrencia n l = if (length [ x | x <- l, x == n]) == 1 then True else False

--Questão 12
intercala :: [Int] -> [Int] -> [Int]
intercala [] l = l
intercala l [] = l
intercala (x:xs) (y:ys) = x: intercala (y:ys) xs

--Questão 13
zipar :: [Int] -> [Int] -> [[Int]]
zipar [] _ = []
zipar _ [] = []
zipar (x:xs) (y:ys) = [x,y]:zipar xs ys

--Questão 14
type Agenda = (String, String, Integer, String)
contatos :: [Agenda]
contatos = [("Matheus", "Uberlandia", 34991997155, "matheuscbrito@hotmail.com"),
            ("Brito", "Uberaba", 34999999999, "brito@ufu.br"),
            ("Teste", "Uberlandia", 34988888888, "teste@ufu.br")]
recupera :: String -> [Agenda] -> String
recupera _ [] = "Email desconhecido"
recupera mail l = if mail == (getmail (head l)) then (getnom (head l)) else (recupera mail (tail l))
                    where
                        getmail (_, _, _, m) = m
                        getnom (n, _, _, _) = n

--Questão 15
type Pessoa = (String, Float, Int, Char)
pessoas :: [Pessoa]
pessoas = [ ("Rosa", 1.66, 27,'F'),
            ("João", 1.85, 26, 'C'),
            ("Maria", 1.55, 62, 'S'),
            ("Jose", 1.78, 42, 'C'),
            ("Paulo", 1.93, 25, 'S'),
            ("Clara", 1.70, 33, 'C'),
            ("Bob", 1.45, 21, 'C'),
            ("Rosana", 1.58,39, 'S'),
            ("Daniel", 1.74, 72, 'S'),
            ("Jocileide", 1.69, 18, 'S') ]

altmedia :: [Pessoa] -> Float 
altmedia l = (sum [(getalt x) | x <- l])/(fromIntegral (length l))
             where getalt (_, a, _, _) = a

mais_nova :: [Pessoa] -> Int
mais_nova [p, p2] = if (getid p) <= (getid p2) then (getid p) else (getid p2)
                    where getid (_, _, i, _) = i
mais_nova (p:p2:r) = if (getid p) <= (getid p2) then (mais_nova ([p]++r)) else (mais_nova ([p2]++r))
                        where getid (_, _, i, _) = i

mais_velha :: [Pessoa] -> (String, Char)
mais_velha [p, p2] = if (getid p) >= (getid p2) then ((getnome p), (getec p)) else ((getnome p2), (getec p2))
                        where 
                            getid (_, _, i, _) = i
                            getnome (n, _, _, _) = n
                            getec (_, _, _, e) = e
mais_velha (p:p2:r) = if (getid p) >= (getid p2) then (mais_velha ([p]++r)) else (mais_velha ([p2]++r))
                        where 
                            getid (_, _, i, _) = i
                            getnome (n, _, _, _) = n
                            getec (_, _, _, e) = e
                        
dados :: [Pessoa] -> [Pessoa]
dados l = [ x | x <- l, (getid x) >= 50]
        where getid (_, _, i, _) = i

casados :: Int -> [Pessoa] -> Int
casados i l = (length [ x | x <- l, (getid x) > i && (getec x) == 'C'])
            where             
                getid (_, _, i, _) = i
                getec (_, _, _, e) = e

--Questão 16
insere_ord :: Ord t => t -> [t] -> [t]
insere_ord x [] = [x]
insere_ord x (y:ys)
    | x <= y = [x]++(y:ys)
    | otherwise = y: insere_ord x ys

--Questão 17
reverte :: [t] -> [t]
reverte [] = []
reverte (x:xs) = (reverte xs)++[x]

--Questão 18
elimina_repet :: Eq t => [t] -> [t]
elimina_repet [] = []
elimina_repet (x:xs) = if (null l) == True then [x]++(elimina_repet xs) else (elimina_repet xs)
                        where l = [l | l <- xs, l == x]

--Questão 19
disponiveis = [1,2,5,10,20,50,100]
notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco x = [ y:ys | y <- disponiveis, x >= y, ys <- notasTroco(x - y)]

--Questão 20
nRainhas :: Int -> [[Int]]
nRainhas n = solucao n
    where 
        solucao 0 = [[]] -- Caso Base
        solucao x = [y:ys | ys <- solucao (x-1), y <- [1..n], seguro y ys] --retorna as lista de soluções do tabuleiro nxn para as posições seguras
        seguro y ys = and [not (captura y ys i) | i <- [0..(length ys-1)]] -- Verifica se a posição é segura e retorna a lista só das posições que não captura
        captura y ys i = y == (ys!!i) || abs (y - (ys!!i)) == i+1 -- Verifica se captura na mesma linha, coluna e na diagonal