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

maiorValor:: [Int] -> Int
maiorValor [a] = a 
maiorValor (a:x) = max a (maiorValor x)


--Exercicio 4

inverterLista :: [a] -> [a]
inverterLista lista = inverterAux lista []

inverterAux :: [a] -> [a] -> [a]
inverterAux [] acc = acc
inverterAux (x:xs) acc = inverterAux xs (x:acc)

--Exercicio 5

ultimoElemento:: [Int] -> Int
ultimoElemento [a] = a 
ultimoElemento (a:x) = ultimoElemento x


--Exercicio 6

kEsimoElemento:: [Int] -> Int -> Int
kEsimoElemento (a:x) n 
    | n == 1 = a 
    | otherwise = kEsimoElemento x (n-1)

