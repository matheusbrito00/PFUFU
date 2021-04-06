produtorio :: [Float] -> Float
produtorio [x] = x
produtorio (x:xs) = x * produtorio xs