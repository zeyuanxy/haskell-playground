-- @Author: Zeyuan Shang
-- @Date:   2016-06-01 19:22:05
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-06-01 19:25:14

data Tree a = Node a [Tree a]
    deriving (Eq, Show)

ipl :: Tree a -> Int
ipl = ipl' 0
	where ipl' x (Node _ ts) = x + (sum $ map (ipl' (x + 1)) ts)

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
	let value = ipl tree5
	print value