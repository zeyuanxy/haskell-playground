-- @Author: Zeyuan Shang
-- @Date:   2016-06-07 12:57:23
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-06-14 13:47:56

paths :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths src dst edges
    | src == dst = [[dst]]
    | otherwise = [
        src : path | edge <- edges, (fst edge) == src, 
                     path <- (paths (snd edge) dst [e | e <- edges, e /= edge])]

cycle' :: (Eq a) => a -> [(a, a)] -> [[a]]
cycle' src edges = [
    src : path | edge <- edges, (fst edge) == src,
                 path <- (paths (snd edge) src [e | e <- edges, e /= edge])]

main = do
    let value = cycle' 2 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    print value
    let value2 = cycle' 1 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    print value2