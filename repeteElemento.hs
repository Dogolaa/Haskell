--Ex 8
--Faça uma função que, dada uma lista de inteiros, retorna uma lista com repetição de cada elemento de
--acordo com seu valor.
--Hugs > repeteElemento [1,2,3,4,5]repet
--[1,2,2,3,3,3,4,4,4,4,5,5,5,5,5]

repeteElemento :: [Int] -> [Int]
repeteElemento [] = []
repeteElemento (a:b) = (repete a a)++(repeteElemento b)

repete :: Int -> Int -> [Int]
repete 0 _ = []
repete _ 0 = []
repete a b = a:(repete a (b-1))