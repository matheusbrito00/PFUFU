dist :: (Float, Float) -> (Float, Float) -> Float
dist (xA, yA) (xB, yB) = sqrt (((xB - xA)^2) + ((yB - yA)^2))

distEspaco :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distEspaco (xA, yA, zA) (xB, yB, zB) = sqrt (((xB - xA)^2) + ((yB - yA)^2) + ((zB - zA)^2))