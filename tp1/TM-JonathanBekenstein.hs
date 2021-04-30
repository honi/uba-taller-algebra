-- Taller de Álgebra 1: Primer cuatrimestre 2021
-- Trabajo práctico 1: La conjetura de Goldbach
-- Nombre: Jonathan Bekenstein
-- DNI: 32757221
-- Libreta: 348/11


-- Recibe un número natural n y devuelve True si y solo sí el n es par,
-- mayor que 2 y suma de dos números primos o False en caso contrario.
satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n
    | n `mod` 2 == 1 || n <= 2 = False
    | otherwise = descomposicionEnPrimos n /= (0, 0)


-- Recibe un número natural n par mayor que 2 y devuelve True si y solo sí
-- la conjetura es cierta para todos los naturales pares mayores que 2
-- y menores o iguales que n o False en caso contrario.
verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n
    | n == 4 = True
    | satisfaceGoldbach n = verificarConjeturaHasta (n-2)


-- Recibe un número natural n par mayor que 2 y devuelve un
-- par ordenado (a,b) de números primos tales que a + b == n.
descomposicionEnPrimos :: Integer -> (Integer, Integer)
descomposicionEnPrimos n = descomposicionEnPrimosAux n 2
    where
        descomposicionEnPrimosAux :: Integer -> Integer -> (Integer, Integer)
        descomposicionEnPrimosAux n i
            -- Por más que confiemos en Goldbach, planteo un caso base por si
            -- no se cumple la conjetura. Considero el (0, 0) como un "False".
            | i == n = (0, 0)
            | esPrimo i && esPrimo (n-i) = (i, n-i)
            | otherwise = descomposicionEnPrimosAux n (i+1)


-- Recibe un número natural n par mayor que 2 y devuelve la cantidad de pares
-- ordenados (a, b) de números primos tales que a + b == n.
numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n = numeroDeDescomposicionesAux n 2
    where
        numeroDeDescomposicionesAux :: Integer -> Integer -> Integer
        numeroDeDescomposicionesAux n i
            | i == nDiv2 && sonPrimos = 1
            | i == nDiv2 = 0
            | sonPrimos = 2 + numeroDeDescomposicionesAux n (i+1)
            | otherwise = numeroDeDescomposicionesAux n (i+1)
            where
                nDiv2 = n `div` 2
                sonPrimos = esPrimo i && esPrimo (n-i)


-- Función auxiliar para verificar si un número es primo.
-- Utilizo la propiedad de que si un número no es primo, entonces uno de sus
-- posibles factores se encuentra entre 2 y su raíz cuadrada.
-- Ref: https://stackoverflow.com/a/5811176/1390293
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n (sqrt (fromIntegral n)) 2
    where
        esPrimoAux :: Integer -> Float -> Integer -> Bool
        esPrimoAux n sqrtn i
            | fromIntegral i > sqrtn = True
            | n `mod` i == 0 = False
            | otherwise = esPrimoAux n sqrtn (i+1)
