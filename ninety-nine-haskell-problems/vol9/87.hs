-- @Author: Zeyuan Shang
-- @Date:   2016-07-28 23:49:48
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-07-29 00:14:18

data Graph a = Graph [a] [(a, a)]
    deriving (Show, Eq)

depthfirst :: (Eq a) => Graph a -> a -> [a]
depthfirst graph src = depthfirst' graph [src]

depthfirst' :: (Eq a) => Graph a -> [a] -> [a]
depthfirst' (Graph [] _) _ = []
depthfirst' (Graph _ _) [] = []
depthfirst' (Graph vs es) (x:xs) = x : depthfirst' (Graph vs' es) (xs' ++ xs)
    where vs' = filter (x /=) vs
          xs' = [b | (a, b) <- es, a == x, elem b vs] ++ [a | (a, b) <- es, b == x, elem a vs]

main = do
    print $ depthfirst (Graph [1,2,3,4,5] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4)]) 1    