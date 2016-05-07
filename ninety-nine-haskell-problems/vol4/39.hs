isPrime :: Int -> Bool
isPrime n = if n <= 1 then False
            else all (\x -> n `mod` x /= 0) [2 .. s]
            where s = floor . sqrt . fromIntegral $ n 

primesR :: Int -> Int -> [Int]
primesR x y = filter isPrime [x..y]

main :: IO ()
main = do
    x <- getLine
    y <- getLine
    let value = primesR (read x) (read y)
    print value
