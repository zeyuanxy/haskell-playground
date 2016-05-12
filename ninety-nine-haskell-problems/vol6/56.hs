-- @Author: Zeyuan Shang
-- @Date:   2016-05-12 22:17:47
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-12 22:29:14

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l r) (Branch _ ll rr) = (mirror l rr) && (mirror r ll)
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

main = do
    print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty))
    print $ symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)