-- Exercicio 1
primeirosN :: Int -> [a] -> [a]
primeirosN n _ | n <= 0 = []  
primeirosN _ [] = []        
primeirosN n (x:xs) = x : primeirosN (n-1) xs  

--Exercicio 2

existeNumero :: Eq a => a -> [a] -> Bool
existeNumero _ [] = False  
existeNumero num (x:xs)
    | num == x  = True      
    | otherwise = existeNumero num xs

--Exercicio 3

maiorValor :: (Ord a, Num a) => a -> [a] -> a
maiorValor _ [] = 0 
maiorValor padrao (x:xs) = foldl max x xs


foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs


max :: (Ord a) => a -> a -> a
max a b | a >= b    = a
        | otherwise = b


--Exercicio 4

inverterLista :: [a] -> [a]
inverterLista lista = inverterAux lista []

inverterAux :: [a] -> [a] -> [a]
inverterAux [] acc = acc
inverterAux (x:xs) acc = inverterAux xs (x:acc)

--Exercicio 5

ultimoElemento :: a -> [a] -> a
ultimoElemento padrao [] = padrao
ultimoElemento _ [x] = x
ultimoElemento padrao (_:xs) = ultimoElemento padrao xs

--Exercicio 6

kEsimoElemento :: Int -> a -> [a] -> a
kEsimoElemento k padrao lista
    | k < 0 = padrao  
    | otherwise = encontraK k lista

encontraK :: Int -> [a] -> a
encontraK 0 (x:_) = x             
encontraK k (_:xs)
    | k > 0 = encontraK (k - 1) xs  
encontraK _ [] = error "Índice fora dos limites da lista"