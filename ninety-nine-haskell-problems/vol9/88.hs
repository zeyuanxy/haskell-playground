-- @Author: Zeyuan Shang
-- @Date:   2016-07-28 23:49:48
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-07-29 20:35:29

data Graph a = Graph [a] [(a, a)]
    deriving (Show, Eq)

depthfirst :: (Eq a) => Graph a -> a -> [a]
depthfirst graph src = depthfirst' graph [src]

depthfirst' :: (Eq a) => Graph a -> [a] -> [a]
depthfirst' (Graph [] _) _ = []
depthfirst' (Graph _ _) [] = []
depthfirst' (Graph vs es) (x:xs) = if elem x vs then x : depthfirst' (Graph vs' es) (xs' ++ xs)
                                   else depthfirst' (Graph vs' es) (xs' ++ xs)
    where vs' = filter (x /=) vs
          xs' = [b | (a, b) <- es, a == x, elem b vs] ++ [a | (a, b) <- es, b == x, elem a vs]

connectedcomponents :: (Eq a) => Graph a -> [[a]]
connectedcomponents (Graph [] _) = []
connectedcomponents (Graph vs es) = component : connectedcomponents (Graph vs' es)
    where component = depthfirst (Graph vs es) (head vs) 
          vs' = filter (\x -> not (elem x component)) vs

main = do
    print $ connectedcomponents (Graph [1,2,3,4,5,6,7] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)])