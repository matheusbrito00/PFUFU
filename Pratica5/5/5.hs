tamanho :: [t] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs