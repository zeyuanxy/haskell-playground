isPrime :: Int -> Bool
isPrime n = if n <= 1 then False
            else all (\x -> n `mod` x /= 0) [2 .. s]
            where s = floor . sqrt . fromIntegral $ n 

main :: IO ()
main = do
    n <- getLine
    let value = isPrime . read $ n
    print value
