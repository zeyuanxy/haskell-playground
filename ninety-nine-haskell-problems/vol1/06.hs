isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

main :: IO ()
main = do
    let last = isPalindrome [1, 2, 2, 1]
    print last
