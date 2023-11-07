quantosDias :: Int -> Int
quantosDias a
    | mod a 4 == 0 = 366
    | otherwise = 365

diasMes :: Int -> Int -> Int    
diasMes a 2
    | quantosDias a == 366 = 29
    | otherwise = 28
diasMes _ b
    |(b == 2 || b == 4 || b == 6 || b == 9 || b == 11) = 30
    |otherwise = 31
