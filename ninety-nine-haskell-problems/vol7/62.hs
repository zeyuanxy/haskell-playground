data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

internals :: Tree a -> [a]
internals Empty = []
internals (Branch x Empty Empty) = []
internals (Branch x l r) = x : (internals l ++ internals r)

main = do
    let value = internals (Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty))
    print value

