-- @Author: Zeyuan Shang
-- @Date:   2016-07-23 16:53:46
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-07-23 17:18:19
-- enlightened from https://wiki.haskell.org/99_questions/Solutions/84
import Data.List

data Graph a = Graph [a] [(a, a, a)]
    deriving (Show, Eq)

graph = Graph [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]

prim :: (Num a, Ord a) => Graph a -> a
prim (Graph vs es) = sum . map (\(w, x, y) -> w) $ prim' [(head vs)] (tail vs) [] 
    where
        prim' curr [] mst = mst
        prim' left right mst = 
            let e@(w, x, y) = minimum [(w, x, y) | (x, y, w) <- es, elem x left, elem y right]
            in prim' (y:left) (delete y right) (e:mst)

main = do
    print $ prim graph