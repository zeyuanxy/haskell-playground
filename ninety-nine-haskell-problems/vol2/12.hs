data Element a = Single a | Multiple Int a
    deriving (Show)

decodeModified :: (Eq a) => [Element a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = x : (decodeModified xs)
decodeModified ((Multiple n x):xs) = (replicate n x) ++ (decodeModified xs)

main :: IO ()
main = do
    let value = decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
    print value
