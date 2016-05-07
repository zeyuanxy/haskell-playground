coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

totient :: Int -> Int
totient n = length [x | x <- [1..n], coprime n x]

main :: IO ()
main = do
    n <- getLine
    let value = totient (read n)
    print value
