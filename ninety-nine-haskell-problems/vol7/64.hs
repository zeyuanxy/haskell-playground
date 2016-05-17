import Data.Maybe

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

type Pos = (Int, Int)

layout :: Tree a -> Tree (a, Pos)
layout t = fst $ layout' 1 1 t
    where layout' x y Empty = (Empty, x)
          layout' x y (Branch root l r) = (Branch (root, (x', y)) l' r', x'')
            where (l', x') = layout' x (y + 1) l 
                  (r', x'') = layout' (x' + 1) (y + 1) r

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

main = do
    let value = layout tree64
    print value