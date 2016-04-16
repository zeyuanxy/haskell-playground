myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

main :: IO ()
main = do
    let last = myReverse [1, 2, 3, 4]
    print last
