ordena :: [Int] -> [Int]
ordena [] = []  -- Lista vazia já está ordenada
ordena (x:xs) = insere x (ordena xs)

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y:ys)
    | x <= y = x : y : ys
    | otherwise = y : insere x ys
