compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:ys@(y:_)) = if x == y then compress ys
                        else x : compress ys

main :: IO ()
main = do
    let value = compress "aaaabccaadeeee"
    print value
