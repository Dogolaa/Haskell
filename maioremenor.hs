maioremenor :: [Int] -> (Int,Int)
maioremenor [a] = (a,a)
maioremenor (a:x) = (menor a(maioremenor x),(maior a (maioremenor x)))


maior :: Int -> Int -> Int
maior a b
    |a < b = b
    |otherwise = a

menor :: Int -> Int -> Int
menor a b
    |a > b = b
    |otherwise = a