-- @Author: Zeyuan Shang
-- @Date:   2016-05-12 22:17:47
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-12 22:24:41

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [Branch 'x' l r | i <- [q..q+r], l <- cbalTree i, r <- cbalTree (n - 1 - i)]
    where (q, r) = (n - 1) `quotRem` 2

main = do
    let value = cbalTree 4
    print value