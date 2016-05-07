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

phi :: Int -> Int
phi m = product [(p - 1) * p ^ (n - 1) | (p, n) <- prime_factors_mult m]

main :: IO ()
main = do
    n <- getLine
    let value = phi (read n)
    print value
