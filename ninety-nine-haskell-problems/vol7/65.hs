import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout t = fst $ layout' 1 1 (depth t) t
    where layout' x y d Empty = (Empty, x)
          layout' x y d (Branch root l r) = (Branch (root, (x', y)) l' r', x' + s * 2)
            where (l', x') = layout' x (y + 1) (d - 1) l 
                  (r', x'') = layout' (x' + s + 1) (y + 1) (d - 1) r
                  s = if d > 1 then 2 ^ (d - 2) else 0
          depth Empty = 0
          depth (Branch _ l r) = 1 + max (depth l) (depth r)

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