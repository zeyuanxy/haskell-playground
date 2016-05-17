import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = completeBinaryTree' 1
    where completeBinaryTree' x
            | x > n = Empty
            | otherwise = Branch 'x' (completeBinaryTree' (2 * x)) (completeBinaryTree' (2 * x + 1))

traverseTree :: Tree Char -> Maybe (Int, Bool)
traverseTree Empty = Just (0, True)
traverseTree (Branch _ l r) = do
    (hl, cl) <- traverseTree l
    (hr, cr) <- traverseTree r
    if (hl == hr && cl) then Just (1 + hl, cr)
    else if (hl == hr + 1 && cr) then Just (1 + hl, False)
    else Nothing

isCompleteBinaryTree :: Tree Char -> Bool
isCompleteBinaryTree = (/= Nothing) . traverseTree

main = do
    let value1 = completeBinaryTree 4
    print value1
    let value2 = isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
    print value2
    let value3 = isCompleteBinaryTree $ Branch 'x' (Branch 'x' Empty (Branch 'x' Empty Empty)) (Branch 'x' Empty Empty)
    print value3