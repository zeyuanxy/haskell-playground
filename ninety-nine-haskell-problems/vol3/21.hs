insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = let (left, right) = splitAt (n - 1) xs
                  in left ++ (x : right)

main :: IO ()
main = do
    let value = insertAt 'X' "abcd" 2
    print value
