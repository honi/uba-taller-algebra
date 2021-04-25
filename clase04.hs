module Clase04 where
    import Clase03(digitosTodosIguales)

    f1 :: Int -> Int
    f1 0 = 1
    f1 n = 2^n + f1 (n-1)

    f1Test :: Int -> Bool
    f1Test n = f1 n == 2^(n+1) - 1

    f2 :: Int -> Float -> Float
    f2 0 q = 0
    f2 n q = q^n + f2 (n-1) q

    f2Test :: Int -> Float -> Bool
    f2Test n q = f2 n q == (q^(n+1) - q) / (q-1)

    f3 :: Int -> Float -> Float
    f3 0 q = 0
    f3 n q = q^(n*2) + q^(n*2-1) + f3 (n-1) q

    f3Test :: Int -> Float -> Bool
    f3Test n q = f3 n q == (q^(n*2 + 1) - q) / (q-1)

    f4 :: Int -> Float -> Float
    f4 0 q = 1
    f4 n q = q^(n*2) + q^(n*2-1) - q^(n-1) + f4 (n-1) q

    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = n * factorial (n-1)

    eAprox :: Int -> Float
    eAprox 0 = 1
    eAprox n = 1 / fromIntegral(factorial n) + eAprox (n-1)

    e :: Float
    e = eAprox 10

    fnm :: Int -> Int -> Int
    fnm 0 m = 0
    fnm n m = round(f2 m (fromIntegral n)) + fnm (n-1) m

    sumaPotencias :: Float -> Int -> Int -> Float
    sumaPotencias q 0 m = 0
    sumaPotencias q n m = f2 (n + m) q - f2 n q + sumaPotencias q (n-1) m

    sumaRacionalesAux :: Int -> Int -> Float
    sumaRacionalesAux 0 m = 0
    sumaRacionalesAux n m = fromIntegral n / fromIntegral m + sumaRacionalesAux (n-1) m

    sumaRacionales :: Int -> Int -> Float
    sumaRacionales n 0 = 0
    sumaRacionales n m = sumaRacionalesAux n m + sumaRacionales n (m-1)

    g1 :: Int -> Int -> Int
    g1 i n
        | n > i = i^n + g1 i (n-1)
        | n < i = 0
        | otherwise = i^n

    g2Aux :: Int -> Int -> Int
    g2Aux 0 n = 0
    g2Aux b n = b^n + g2Aux (b-1) n

    g2 :: Int -> Int
    g2 0 = 0
    g2 n = g2Aux n n + g2 (n-1)

    g3 :: Int -> Int
    g3 0 = 0
    g3 n = (1 - n `mod` 2) * 2^n + g3 (n-1)

    sumaDigitosIguales :: Int -> Int
    sumaDigitosIguales n
        | n == 0 = 0
        | digitosTodosIguales n = n + sumaDigitosIguales (n-1)
        | otherwise = sumaDigitosIguales (n-1)
