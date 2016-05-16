data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x l r) depth
	| depth == 1 = [x]
	| depth > 1 = atLevel l (depth - 1) ++ atLevel r (depth - 1)
	| otherwise = []


main = do
    let value = atLevel (Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)) 2
    print value

