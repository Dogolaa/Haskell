--Lucas Dogo de Souza Pezzuto
--2020.1.08.026
--Lista 2


--Ex 1
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n-1], n `mod` x == 0]

ehPerfeito :: Int -> Bool
ehPerfeito n = sum (divisores n) == n

--Ex 2

contaString:: String -> [(Char, Int)]
contaString [a] = [(a, 1)]
contaString (a:x) = (a, conta a x):(contaString x)


conta:: Char -> String -> Int
conta a [] = 1
conta a (b:x)
    |a == b = 1 + conta a x
    |otherwise = conta a x


--Ex 3

inverte :: String -> String
inverte [] = []
inverte (a : x) = (inverte x) ++ [a]



--Ex 4 


squares:: Int -> Int 
squares a = a*a

mapSquares:: (Int -> Int) -> [Int] -> [Int]
mapSquares f a = map f a



--Ex 5

cartesiano:: [Int] -> [Int] -> [(Int, Int)]
cartesiano a b = (cartesiano2 a b)++(cartesiano2 b a)

cartesiano2:: [Int] -> [Int] -> [(Int, Int)]
cartesiano2 [] _ = []
cartesiano2 _ [] = []
cartesiano2 (a:x) (b:y) = ((montaCartesiano a (b:y))++(cartesiano2 x (b:y)))

montaCartesiano:: Int -> [Int] -> [(Int, Int)]
montaCartesiano a [b] = [(a,b)] 
montaCartesiano a (b:x) = (a,b):(montaCartesiano a x)


--Ex 6


filterPositives :: (Int -> Bool) -> [Int] -> [Int]
filterPositives f a = filter f a

positives :: Int -> Bool
positives a
    | a > 0 = True
    |otherwise = False


--Ex 7 

sumDouble :: Int -> Int
sumDouble a = 2 * a

mapSumDouble :: (Int -> Int) -> [Int] -> Int
mapSumDouble f a = sum (map f a)


--Ex 8

concatena :: String -> String -> String
concatena a b = a ++ b

foldr1Concatena :: (String -> String -> String) -> [String] -> String
foldr1Concatena f a = foldr1 f a