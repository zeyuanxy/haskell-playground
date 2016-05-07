isPrime :: Int -> Bool
isPrime n = if n <= 1 then False
            else all (\x -> n `mod` x /= 0) [2 .. s]
            where s = floor . sqrt . fromIntegral $ n 

primesR :: Int -> Int -> [Int]
primesR x y = filter isPrime [x..y]

goldbach :: Int -> (Int, Int)
goldbach n = head [(x, y) | x <- primesR 2 n, y <- primesR 2 n, x /= y, x + y == n]

main :: IO ()
main = do
    n <- getLine
    let value = goldbach (read n)
    print value
