encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode (x:xs) = let (first, rest) = span (== x) xs
              in (x, 1 + length first) : encode rest

primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2
    where primeFactors' 1 _ = []
          primeFactors' x factor
            | x `mod` factor == 0 = factor : primeFactors' (x `div` factor) factor
            | otherwise = primeFactors' x (factor + 1)

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = encode . primeFactors $ n

main :: IO ()
main = do
    n <- getLine
    let value = prime_factors_mult 315
    print value
