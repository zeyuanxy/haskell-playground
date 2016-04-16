data Element a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [Element a]
encodeModified [] = []
encodeModified (x:xs) = let (first, rest) = span (== x) xs
                        in if (length first == 0) then (Single x) : (encodeModified rest)
                           else (Multiple (1 + length first) x) : (encodeModified rest)

main :: IO ()
main = do
    let value = encodeModified "aaaabccaadeeee"
    print value
