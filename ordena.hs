--Ex 7
--Faça uma função (ou mais) que recebe uma lista com números e retorna outra lista com os números
--ordenados:
--Hugs > ordena [7, 3, 5, 7, 8, 4, 4]

ordena :: [Int] -> [Int]
ordena [] = []  -- Lista vazia já está ordenada
ordena (x:xs) = insere x (ordena xs)

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y:ys)
    | x <= y = x : y : ys
    | otherwise = y : insere x ys

    

ordena2 :: [Int] -> [Int]
ordena2 [] = []
ordena2 (a:b) = ordena2 [x | x <- b,  x < a] ++ [a] ++ ordena2 [x | x <- b, x >= a]




