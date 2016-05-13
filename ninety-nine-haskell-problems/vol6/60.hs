-- @Author: Zeyuan Shang
-- @Date:   2016-05-12 22:17:47
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-14 00:21:22

import Data.List
import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

hbalTree :: a -> Int -> [Tree a]
hbalTree x n = map fst $ hbalTree' n
    where hbalTree' 0 = [(Empty, 0)]
          hbalTree' 1 = [(Branch x Empty Empty, 1)]
          hbalTree' n = let t = hbalTree' (n - 2) ++ hbalTree' (n - 1)
                        in [(Branch x l r, h) | (l, lh) <- t, (r, rh) <- t, let h = 1 + max lh rh, h == n]

-- copy from https://wiki.haskell.org/99_questions/Solutions/60
hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x n = concatMap toFilteredTrees [minHeight..maxHeight]
	where 
		minHeight = ceiling . logBase 2 $ fromIntegral (n + 1)
		minNodesSeq = 0:1:zipWith ((+).(1+)) minNodesSeq (tail minNodesSeq)
		maxHeight = (fromJust $ findIndex (>n) minNodesSeq) - 1
		countNodes Empty = 0
		countNodes (Branch _ l r) = 1 + countNodes l + countNodes r
		toFilteredTrees = filter ((n==) . countNodes) . hbalTree x

main = do
    let value = hbalTreeNodes 'x' 15
    print value
    print (length value)