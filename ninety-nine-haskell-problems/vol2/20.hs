removeAt :: [a] -> Int -> (a, [a])
removeAt xs n = (xs !! (n - 1), (take (n - 1) xs) ++ (drop n xs))

main :: IO ()
main = do
    let value = removeAt "abcd" 2
    print value
