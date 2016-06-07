-- @Author: Zeyuan Shang
-- @Date:   2016-06-07 12:57:23
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-06-07 13:02:57

paths :: (Eq a) => a -> a -> [(a, a)] -> [[a]]
paths src dst edges
    | src == dst = [[dst]]
    | otherwise = [
        src : path | edge <- edges, (fst edge) == src, 
                     path <- (paths (snd edge) dst [e | e <- edges, e /= edge])]

main = do
    let value = paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    print value