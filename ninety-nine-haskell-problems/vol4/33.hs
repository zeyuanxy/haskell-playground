myGCD :: Int -> Int -> Int
myGCD x y | y == 0 = abs x
          | otherwise = myGCD y (x `mod` y)

main :: IO ()
main = do
    x <- getLine
    y <- getLine
    let value = myGCD (read x) (read y)
    print value
