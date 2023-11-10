repeteElemento :: [Int] -> [Int]
repeteElemento [] = []
repeteElemento (a:x) = (repete a a) ++ (repeteElemento x)

repete :: Int -> Int -> [Int]
repete _ 0 = []
repete 0 _ = []
repete a b = a : (repete a (b-1))


ordena :: [Int] -> [Int]
ordena [] = []
ordena (cabeca:cauda) = ordena [x | x <- cauda, x < cabeca] ++ [cabeca] ++ ordena [x | x <- cauda, x >= cabeca]