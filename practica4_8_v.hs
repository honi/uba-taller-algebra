calcular :: Int -> (Int, Int, Int, Int)
calcular n = (a, b, k, r)
    where
        a = n^2 + 5
        b = n+2
        k = a `div` b
        r = a `mod` b
