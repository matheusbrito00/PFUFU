type Data = (Int, Int, Int)

precede :: Data -> Data -> Bool
precede (d1, m1, a1) (d2, m2, a2)
    | a1 < a2 = True
    | a1 == a2 && m1 < m2 = True
    | a1 == a2 && m1 == m2 && d1 < d2 = True
    | otherwise = False
