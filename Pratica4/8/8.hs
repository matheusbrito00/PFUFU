bissexto :: Int -> Bool
bissexto x 
    | mod x 4 == 0 && (mod x 100 /= 0 || mod x 400 == 0) = True
    | otherwise = False

bissextos :: [Int] -> [Int]
bissextos a = [ x | x <- a, bissexto x == True]