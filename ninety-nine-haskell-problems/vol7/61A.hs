data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

main = do
    let value = leaves (Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty))
    print value

