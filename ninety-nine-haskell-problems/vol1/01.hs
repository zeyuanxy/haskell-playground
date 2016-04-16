myLast :: [a] -> a
myLast [] = error "No Last!"
myLast [x] = x
myLast (_:xs) = myLast xs

main :: IO ()
main = do
    let last = myLast [1, 2, 3, 4]
    print last
