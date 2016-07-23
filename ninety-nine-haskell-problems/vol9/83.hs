-- @Author: Zeyuan Shang
-- @Date:   2016-07-23 15:33:02
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-07-23 16:47:59
import Data.List

data Graph a = Graph [a] [(a, a)]
    deriving (Show, Eq)

k4 = Graph ['a', 'b', 'c', 'd']
    [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]

-- from 81
paths :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths src dst edges
    | src == dst = [[dst]]
    | otherwise = 
        [src : path | edge <- edges, (fst edge) == src,
                      path <- (paths (snd edge) dst [e | e <- edges, e /= edge])] ++
        [src : path | edge <- edges, (snd edge) == src, 
                      path <- (paths (fst edge) dst [e | e <- edges, e /= edge])]

-- from 82
cycle' :: (Eq a) => a -> [(a, a)] -> [[a]]
cycle' a xs = [a : path | e <- xs, fst e == a, path <- paths (snd e) a [x | x <- xs, x /= e]] ++
              [a : path | e <- xs, snd e == a, path <- paths (fst e) a [x | x <- xs, x /= e]]

spantree :: (Eq a) => Graph a -> [Graph a]
spantree (Graph vertices edges) = filter (connected) $ filter (not . cycles) $ filter (nodes) alltrees
    where 
        numVertices = length vertices
        numEdges = length edges
        alltrees = [Graph vertices sub_edges | sub_edges <- foldr (\e es -> es ++ (map (e:) es)) [[]] edges]
        nodes (Graph xs' ys') = numVertices == length xs'
        cycles (Graph xs' ys') = any ((/=) 0 . length . flip cycle' ys') xs'
        connected (Graph (x':xs') ys') = not $ any (null) [paths x' y' ys' | y' <- xs']
        
main = do
    print $ length $ spantree k4