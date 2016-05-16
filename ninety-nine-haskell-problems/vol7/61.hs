data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = 1 + countLeaves l + countLeaves r

main = do
    let value = countLeaves (Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty))
    print value

