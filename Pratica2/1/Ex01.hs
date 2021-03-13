dobro :: Float -> Float
dobro x = x * 2
--dobr :: Int -> Int
dobr :: Integer -> Integer
dobr x = x + x

quadri :: Float -> Float
quadri x = dobro x * 2
quadru :: Integer -> Integer
quadru x = dobr x + dobr x

hipo :: Float -> Float -> Float
hipo x y = sqrt (x^2 + y^2)

dist :: Float -> Float -> Float -> Float -> Float
dist xa ya xb yb = sqrt (((xb - xa)^2) + ((yb - ya)^2))