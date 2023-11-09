--Ex 9
--Faça uma função que calcula a série:
-- serie = (1/x)+(x/2)+(3/x)+(x/4)
--Você deve passar dois números por parâmetro: o primeiro contendo o valor de x e o segundo o
--número de elementos da série (Para facilitar, use somente o valor inteiro – ignore as casas decimais).
--Main> serie 1 100
--2500
--Main> serie 2 100
--1226

serie :: Int -> Int -> Int
serie _ 0 = 0
serie a 1 = div 1 a
serie a b
    | mod b 2 == 0 = div a b + (serie a (b-1))
    | otherwise = div b a + (serie a (b-1)) 