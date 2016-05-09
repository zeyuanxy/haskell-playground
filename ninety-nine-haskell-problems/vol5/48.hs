import Control.Monad(replicateM)

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

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
infixl 3 `equ'`

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr a ++ show (f a) | a <- args n]
    where args n = replicateM n [True, False]
          toStr = unwords . map (\x -> show x ++ space x)
          space True = "  "
          space False = " "

main = tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)