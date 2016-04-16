elementAt :: [a] -> Int -> a
elementAt elements index = elements !! (index - 1)

main :: IO ()
main = do
    let last = elementAt [1, 2, 3, 4] 2
    print last
