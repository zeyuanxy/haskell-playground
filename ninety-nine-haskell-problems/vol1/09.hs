pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, rest) = span (== x) xs
              in (x : first) : pack rest

main :: IO ()
main = do
    let value = pack "aaaabccaadeeee"
    print value
