myButLast :: [a] -> a
myButLast = last . init

main :: IO ()
main = do
    let last = myButLast [1, 2, 3, 4]
    print last
