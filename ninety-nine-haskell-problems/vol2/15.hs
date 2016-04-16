repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

main :: IO ()
main = do
    let value = repli [1, 2, 3] 3
    print value
