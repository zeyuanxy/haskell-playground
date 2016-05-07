coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

main :: IO ()
main = do
    x <- getLine
    y <- getLine
    let value = coprime (read x) (read y)
    print value
