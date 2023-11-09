--Ex 5
--Defina a função dia que, dados um ano, um mês e um dia do mês, devolve o número de ordem desse
--dia nesse ano. Se os dados não constituírem uma data válida, a função deve devolver -1.



quantosDias :: Int -> Int
quantosDias a
    | mod a 100 == 0 && mod a 400 /= 0 = 365
    | mod a 4 == 0 = 366
    | otherwise = 365


diasMes :: Int -> Int -> Int
diasMes a 2 
    | quantosDias a == 366 = 29
    | otherwise = 28
diasMes _ b 
    | b == 1 ||  b == 3 || b == 5 || b == 7 || b == 8 || b == 10 || b == 12 = 31
    | otherwise = 30






dia :: Int -> Int -> Int -> Int
dia ano mes dia
    | not (dataValida ano mes dia) = -1
    | otherwise = somaDias ano mes dia

dataValida :: Int -> Int -> Int -> Bool
dataValida ano mes dia
    | mes < 1 || mes > 12 || dia < 1 = False
    | mes == 2 = dia <= diasMes ano mes
    | otherwise = dia <= diasMes ano mes

somaDias :: Int -> Int -> Int -> Int
somaDias ano mes dia = sum [diasMes ano m | m <- [1..mes-1]] + dia
