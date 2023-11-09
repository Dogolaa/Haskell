-- Ex 6
-- Sem utilizar as funções max e min, faça uma função que receba uma lista e devolva uma dupla
-- contendo o menor e o maior elemento
-- Hugs > maioremenor [2,5,1,6,9,3,0,4]

maioremenor :: [Int] -> (Int,Int)
maioremenor (a:[]) = (a, a)
maioremenor a = (menorLista a, maiorLista a)



menorLista :: [Int] -> Int
menorLista [a] = a
menorLista (a:x) = menor a (menorLista x)


menor :: Int -> Int -> Int
menor a b 
    | a > b = b    
    | otherwise = a

maiorLista :: [Int] -> Int
maiorLista [a] = a
maiorLista (a:x) = maior a (maiorLista x)

maior :: Int -> Int -> Int
maior a b 
    | a > b = a    
    | otherwise = b
