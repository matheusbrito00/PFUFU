pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if (a == x) then True
                                else pertence a z

uniaoRec2 :: Eq t => [t] -> [t] -> [t]
uniaoRec2 l [] = l
uniaoRec2 l (x:xs) = if pertence x l then uniaoRec2 l xs
                                    else (uniaoRec2 (l++[x]) xs)