-- @Author: Zeyuan Shang
-- @Date:   2016-05-22 22:33:29
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-22 22:35:26

data Tree a = Node a [Tree a]
    deriving (Eq, Show)

nnodes :: Tree a -> Int
nnodes (Node _ ts) = 1 + sum (map nnodes ts)

main = do
    let value = nnodes (Node 'a' [Node 'b' []])
    print value
