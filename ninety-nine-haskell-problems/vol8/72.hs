-- @Author: Zeyuan Shang
-- @Date:   2016-06-01 19:22:05
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-06-01 19:54:44

data Tree a = Node a [Tree a]
    deriving (Eq, Show)

bottom_up :: Tree a -> [a]
bottom_up (Node x ts) = concatMap bottom_up ts ++ [x]

tree1 = Node 'a' []
 
tree2 = Node 'a' [Node 'b' []]
 
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
 
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
 
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

main = do
	let value = bottom_up tree5
	print value