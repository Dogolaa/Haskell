--Faça uma função que verifique se um número inteiro de entrada é primo. 
--Lembrando: O número primo é divisível apenas por 1 e por ele mesmo. 
--O número 1 (um) não é primo e o número 2 (dois) é o único número par que é primo. 
--Hugs > ehPrimo 37 
--True.

ehPrimo :: Int -> Bool
ehPrimo n 
    | n < 2 = False
    | n == 2 = True
    | otherwise = verifica n 2


verifica :: Int -> Int -> Bool
verifica a b 
    | b == a = True
    | mod a b == 0 = False
    | otherwise = verifica a (b+1)
