primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2
    where primeFactors' 1 _ = []
          primeFactors' x factor
            | x `mod` factor == 0 = factor : primeFactors' (x `div` factor) factor
            | otherwise = primeFactors' x (factor + 1)

main :: IO ()
main = do
    n <- getLine
    let value = primeFactors 315
    print value
