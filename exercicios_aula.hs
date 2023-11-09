comp_lista :: [t] -> Int
comp_lista [] = 0
comp_lista (a:x) = 1 + comp_lista x


ordena2 :: [Float] -> [Float]
ordena2 [] = []
ordena2 (a:b) = ordena2 [x | x <- b,  x < a] ++ [a] ++ ordena2 [x | x <- b, x >= a]

retornaNElementos :: [Int] -> Int -> [Int]
retornaNElementos [] _ = []
retornaNElementos _ 0 = []
retornaNElementos (a:x) b = [a] ++ retornaNElementos x (b-1)

inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (a:x) = inverteLista x ++ [a]


agrupaLista :: [Int] -> [Char] -> [(Int, Char)]
agrupaLista [] _ = []
agrupaLista _ [] = []
agrupaLista (a:b) (c:d) = [(a,c)] ++ agrupaLista b d



maiorValor :: [Int] -> Int
maiorValor [] = 0
maiorValor (a:x) = maior a (maiorValor x)

maior :: Int -> Int -> Int
maior a b
    | a < b = b
    |otherwise = a

