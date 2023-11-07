--Construa uma função em Haskell que recebe quatro inteiros e devolva uma tupla-4 com os 
--quatro valores originais, mas ordenados 
--Hugs> ordenaEmTupla 7 0 5 3 
--(0, 3, 5, 7)

ordenaEmTupla :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int)
ordenaEmTupla a b c d = (menor a (menor b (menor c d)), min a (max b ( min c d)) , min a (max b (max c d)), max a (max b (max c d)))

menor :: Int -> Int -> Int
menor a b
    | a > b = b 
    | otherwise = a




