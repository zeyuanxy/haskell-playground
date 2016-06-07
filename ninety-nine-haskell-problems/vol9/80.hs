-- @Author: Zeyuan Shang
-- @Date:   2016-06-07 12:44:38
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-06-07 12:54:45

data Graph a = Graph [a] [(a, a)]
               deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
            deriving (Show, Eq)
 
graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph [] _) = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x, concatMap f ys) : zs)
    where 
        f (a, b) 
            | a == x = [b]
            | b == x = [a]
            | otherwise = []
        Adj zs = graphToAdj (Graph xs ys)

main = do
    let value = graphToAdj $ Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
    print value