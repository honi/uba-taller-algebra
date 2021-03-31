absoluto :: Int -> Int
absoluto n
    | n >= 0 = n
    | otherwise = -n

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y
    | absX >= absY = absX
    | otherwise = absY
    where absX = absoluto x
          absY = absoluto y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z
    | x >= y && y >= z = x -- Estaría bueno que Haskell soporte x >= y >= z
    | y >= z = y
    | otherwise = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y = x == 0 || y == 0

algunoEs0PatternMatching1 :: Float -> Float -> Bool
algunoEs0PatternMatching1 x y
    | x == 0 = True
    | y == 0 = True
    | otherwise = False

algunoEs0PatternMatching2 :: Float -> Float -> Bool
algunoEs0PatternMatching2 0 _ = True
algunoEs0PatternMatching2 _ 0 = True
algunoEs0PatternMatching2 _ _ = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y = x == 0 && y == 0

ambosSon0PatternMatching1 :: Float -> Float -> Bool
ambosSon0PatternMatching1 x y
    | x == 0 && y == 0 = True
    | otherwise = False

ambosSon0PatternMatching2 :: Float -> Float -> Bool
ambosSon0PatternMatching2 0 0 = True
ambosSon0PatternMatching2 _ _ = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10
