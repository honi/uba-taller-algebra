fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

parteEntera :: Float -> Integer
parteEntera x
    | x < 1 = 0
    | otherwise = 1 + parteEntera(x - 1)

esMultiploDeTres :: Int -> Bool
esMultiploDeTres 0 = True
esMultiploDeTres 1 = False
esMultiploDeTres 2 = False
esMultiploDeTres x = esMultiploDeTres(x - 3)

-- |Esta versión acepta números negativos.
esMultiploDeTres2 :: Int -> Bool
esMultiploDeTres2 x
    | x < 0 = esMultiploDeTres2(-x)
    | x == 0 = True
    | x == 1 = False
    | x == 2 = False
    | otherwise = esMultiploDeTres2(x - 3)

sumaImpares :: Int -> Int
sumaImpares 0 = 0
sumaImpares n = 2 * n - 1 + sumaImpares(n - 1)

medioFact :: Int -> Int
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact(n - 2)

sumaDigitos :: Int -> Int
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos(div n 10)

digitosTodosIguales :: Int -> Bool
digitosTodosIguales n
    | n < 10 = True
    | ultimoDigito == anteUltimoDigito = digitosTodosIguales todosMenosElUltimo
    | otherwise = False
    where ultimoDigito = mod n 10
          anteUltimoDigito = mod (div n 10) 10
          todosMenosElUltimo = div n 10
