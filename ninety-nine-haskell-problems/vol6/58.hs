-- @Author: Zeyuan Shang
-- @Date:   2016-05-12 22:17:47
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-12 22:44:11

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [Branch 'x' l r | i <- [q..q+r], l <- cbalTree i, r <- cbalTree (n - 1 - i)]
    where (q, r) = (n - 1) `quotRem` 2

symCbalTrees :: Int -> [Tree Char]
symCbalTrees n = if n `mod` 2 == 0 then []
                 else [Branch 'x' t (mirror t) | t <- cbalTree (n `div` 2)]
                where mirror Empty = Empty
                      mirror (Branch x l r) = Branch x (mirror r) (mirror l)

main = do
    let value = symCbalTrees 5
    print value