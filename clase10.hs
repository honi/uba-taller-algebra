type Set a = [a]

mcd :: Int -> Int -> Int
mcd a 0 = abs a
mcd a b = mcd b (a `mod` b)

mcm :: Int -> Int -> Int
mcm a b = abs (a * b) `div` mcd a b

emcd :: Int -> Int -> (Int, Int, Int)
emcd a 0 = (abs a, 1, 0)
emcd a b = (d, t', s' - a `div` b * t')
    where (d, s', t') = emcd b (a `mod` b)

stmin :: Int -> Int -> (Int, Int)
stmin a b = (s `mod` b, t + s `div` b * a)
    where (_, s, t) = emcd a b

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n (sqrt (fromIntegral n)) 2

esPrimoAux :: Int -> Float -> Int -> Bool
esPrimoAux n sqrtn i
    | fromIntegral i > sqrtn = True
    | n `mod` i == 0 = False
    | otherwise = esPrimoAux n sqrtn (i+1)

--------------------------------------------------------------------------------

ecEquivalente :: (Int, Int, Int) -> (Int, Int, Int)
ecEquivalente (a, b, m)
    | b `mod` d /= 0 = error "No existe solución"
    | otherwise = (a `div` d, b `div` d, m `div` d)
    where d = mcd a m

solucionEc :: (Int, Int, Int) -> (Int, Int)
solucionEc e = (s * b `mod` m, m)
    where
        (a, b, m) = ecEquivalente e
        (_, s, _) = emcd a m

sistemaSimplifEquiv :: [(Int, Int, Int)] -> [(Int, Int)]
sistemaSimplifEquiv es = map solucionEc es

todosLosPrimosMalos :: [(Int, Int)] -> [Int]
todosLosPrimosMalos es = todosLosPrimosMalosDesde es (maximum (map snd es))

todosLosPrimosMalosDesde :: [(Int, Int)] -> Int -> [Int]
todosLosPrimosMalosDesde _ 1 = []
todosLosPrimosMalosDesde es n
    | esPrimo n && contarDivisiones es n >= 2 = n : todosLosPrimosMalosDesde es (n-1)
    | otherwise = todosLosPrimosMalosDesde es (n-1)

contarDivisiones :: [(Int, Int)] -> Int -> Int
contarDivisiones [] _ = 0
contarDivisiones ((r, m):es) n
    | m `mod` n == 0 = 1 + contarDivisiones es n
    | otherwise = contarDivisiones es n

solucSistemaPotenciasPrimo :: [(Int, Int)] -> (Int, Int)
solucSistemaPotenciasPrimo [e] = e
solucSistemaPotenciasPrimo ((r1, m1):(r2, m2):es)
    | m1 <= m2 && r2 `mod` m1 /= r1 = error "No existe solución"
    | m1 <= m2 = solucSistemaPotenciasPrimo ((r2, m2):es)
    | r1 `mod` m2 /= r2 = error "No existe solución"
    | otherwise = solucSistemaPotenciasPrimo ((r1, m1):es)
