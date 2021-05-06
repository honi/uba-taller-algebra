calcular :: Int -> (Int, Int)
calcular b
    | r >= 0 = (k, r)
    | otherwise = (k+1, r-b)
    where
        a = 3 * b + 7
        k = a `div` b
        r = a `mod` b

check :: Int -> Int -> Int -> Bool
check b k r = (3 * b + 7) == k*b + r
