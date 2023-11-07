quantosDias :: Int -> Int
quantosDias a
    | mod a 4 == 0 = 366
    | otherwise = 365