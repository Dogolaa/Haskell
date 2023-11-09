--Ex 8
--Faça uma função que, dada uma lista de inteiros, retorna uma lista com repetição de cada elemento de
--acordo com seu valor.
--Hugs > repeteElemento [1,2,3,4,5]repet
--[1,2,2,3,3,3,4,4,4,4,5,5,5,5,5]

repeteElemento :: [Int] -> [Int]
repeteElemento [] = []
repeteElemento (a:x) = (repete a a) ++ (repeteElemento x)

repete :: Int -> Int -> [Int]
repete _ 0 = []
repete 0 _ = []
repete a b = a : (repete a (b-1))

