import Clase04(factorial)

combinatoria :: Int -> Int -> Int
combinatoria n k = factorial n `div` (factorial k * factorial (n-k))

-- Esta es la sumatoria del ejercicio 25) ii)

sumatoriaAux :: Int -> Int -> Int
sumatoriaAux n 0 = 1
sumatoriaAux n k = combinatoria (n+1) k + sumatoriaAux n (k-1)

sumatoria :: Int -> Int
sumatoria n = sumatoriaAux (2*n) (2*n)

-- Acá estoy probando si otras sumatorias son equivalentes
