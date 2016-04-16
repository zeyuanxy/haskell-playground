rotate :: [a] -> Int -> [a]
rotate xs n = take (length xs) . drop (length xs + n) $ cycle xs

main :: IO ()
main = do
    let value = rotate ['a','b','c','d','e','f','g','h'] 3
    print value
