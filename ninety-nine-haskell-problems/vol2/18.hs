slice :: [a] -> Int -> Int -> [a]
slice xs begin end = take (end - 1) . drop (begin - 1) $ xs

main :: IO ()
main = do
    let value = slice "abcdefghik" 3 7
    print value
