range x y = [x..y]

main :: IO ()
main = do
    let value = range 4 9
    print value
