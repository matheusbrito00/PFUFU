type Data = (Int, Int, Int)

bissexto2 :: Data -> Bool
bissexto2 (d, m, a)
    | mod a 4 == 0 && (mod a 100 /= 0 || mod a 400 == 0) = True
    | otherwise = False