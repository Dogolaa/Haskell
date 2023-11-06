ehPrimo :: Integer -> Bool
ehPrimo n
    | n <= 1 = False
    | n == 2 = True
    | n `mod` 2 == 0 = False
    | otherwise = not $ any (\x -> n `mod` x == 0) [2..intSqrt n]
    where
        intSqrt = floor . sqrt . fromInteger

main :: IO ()
main = do
    print (ehPrimo 2)    -- True
 



