a :: Integer
a = (\x -> \y -> y) ((\z -> z) (\z -> z)) (\w -> w) 5
--5

b :: Integer
b = ((\f -> (\x -> f (f x))) (\y -> (y * y))) 3
--81

c :: Integer
c = ((\f -> (\x -> f (f x))) (\y -> (y + y))) 5
--20

d :: Integer
d = ((\x -> (\y -> x + y) 5) ((\y -> y -3) 7))
--9

e :: Integer
e = (((\f -> (\x -> f (f (f x)))) (\y -> (y * y))) 2)
--256

f :: Integer
f = (\x -> \y -> x + ((\x -> x - 3) y)) 5 6
--8