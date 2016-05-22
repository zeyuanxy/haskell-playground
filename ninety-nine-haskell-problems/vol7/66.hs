import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

type Pos = (Int, Int)

-- copied from https://wiki.haskell.org/99_questions/Solutions/66
layout :: Tree a -> Tree (a, Pos)
layout t = t'
  where (l, t', r) = layout' x' 1 t
        x' = maximum l + 1

        layout' :: Int -> Int -> Tree a -> ([Int], Tree (a, Pos), [Int])
        layout' x y Empty = ([], Empty, [])
        layout' x y (Branch a l r) = (ll', Branch (a, (x, y)) l' r', rr')
          where (ll, l', lr) = layout' (x - sep) (y + 1) l
                (rl, r', rr) = layout' (x + sep) (y + 1) r
                sep = maximum (0:zipWith (+) lr rl) `div` 2 + 1
                ll' = 0 : overlay (map (+sep) ll) (map (subtract sep) rl)
                rr' = 0 : overlay (map (+sep) rr) (map (subtract sep) lr)

overlay :: [a] -> [a] -> [a]
overlay [] ys = ys
overlay xs [] = xs
overlay (x:xs) (y:ys) = x : overlay xs ys

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

main = do
    let value = layout tree65
    print value