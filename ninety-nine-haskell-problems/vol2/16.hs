dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter ((/= n) . snd) $ zip xs (cycle [1..n])

main :: IO ()
main = do
    let value = dropEvery "abcdefghik" 3
    print value
