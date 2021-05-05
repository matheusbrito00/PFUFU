filtrar :: (n -> Bool) -> [n] -> [n]
filtrar checar l = [ x | x <- list, checar x]