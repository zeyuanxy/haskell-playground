-- @Author: Zeyuan Shang
-- @Date:   2016-05-12 22:17:47
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-12 22:39:18

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l r) (Branch _ ll rr) = (mirror l rr) && (mirror r ll)
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

construct :: (Ord a) => [a] -> Tree a
construct [] = Empty
construct xs = foldl iter Empty xs
    where iter t x = insertInto x t
          insertInto x Empty = Branch x Empty Empty
          insertInto x (Branch root l r) = if x < root then Branch root (insertInto x l) r
                                           else Branch root l (insertInto x r)

main = do
    print $ construct [3, 2, 5, 7, 1]
    print $ symmetric . construct $ [5, 3, 18, 1, 4, 12, 21]
    print $ symmetric . construct $ [3, 2, 5, 7, 1]