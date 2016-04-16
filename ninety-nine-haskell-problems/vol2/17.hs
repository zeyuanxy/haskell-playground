split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

main :: IO ()
main = do
    let value = split "abcdefghik" 3
    print value
