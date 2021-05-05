busca :: Int -> [Int] -> (Bool, Int)
busca x lst = if elem x lst then (elem x lst, (length(takeWhile (/=x) lst)) + 1) else (elem x lst, (length(takeWhile (/=x) lst)))