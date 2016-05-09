not' :: Bool -> Bool
not' True = False
not' False = True

and', or', nor', nand', xor', impl', equ' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' False False = False
or' _ _ = True

nor' a b = not' $ or' a b

nand' a b = not' $ and' a b

xor' True False = True
xor' False True = True
xor' _ _ = False

impl' a b = (not' a) `or'` b

equ' a b = not' (xor' a b)

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b)
             | a <- [True, False], b <- [True, False]]

main = table (\a b -> (and' a (or' a b)))
