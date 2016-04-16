data Element a = Single a | Multiple Int a
    deriving (Show)

encode :: Eq a => [a] -> [(Int, a)]
encode = foldr func []
    where func x [] = [(1, x)]
          func x (y@(n', x'):ys) = if x == x' then (1 + n', x') : ys
                                   else (1, x) : y : ys

encodeDirect :: Eq a => [a] -> [Element a]
encodeDirect = map func . encode
    where func (1, x) = Single x
          func (n, x) = Multiple n x

main :: IO ()
main = do
    let value = encodeDirect "aaaabccaadeeee"
    print value
