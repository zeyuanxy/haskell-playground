dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

main :: IO ()
main = do
    let value = dupli [1, 2, 3]
    print value
