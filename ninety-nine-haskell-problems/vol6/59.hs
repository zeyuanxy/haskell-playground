-- @Author: Zeyuan Shang
-- @Date:   2016-05-12 22:17:47
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-12 22:53:47

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

hbalTree :: a -> Int -> [Tree a]
hbalTree x n = map fst $ hbalTree' n
    where hbalTree' 0 = [(Empty, 0)]
          hbalTree' 1 = [(Branch x Empty Empty, 1)]
          hbalTree' n = let t = hbalTree' (n - 2) ++ hbalTree' (n - 1)
                        in [(Branch x l r, h) | (l, lh) <- t, (r, rh) <- t, let h = 1 + max lh rh, h == n]

main = do
    let value = hbalTree 'x' 3
    print value