ehPrimo :: Int -> Bool
ehPrimo a
    | a < 2 = False
    | a == 2 = True
    | otherwise = verifica a 2

verifica :: Int -> Int -> Bool
verifica a b
    | a == b = True
    | mod a b == 0 = False
    | otherwise = verifica a (b+1)
