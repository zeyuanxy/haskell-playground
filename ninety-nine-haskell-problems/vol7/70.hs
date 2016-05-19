-- @Author: Zeyuan Shang
-- @Date:   2016-05-19 22:31:46
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-19 22:44:16
import Text.Parsec.String
import Text.Parsec hiding (Empty)

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

pTree :: Parser (Tree Char)
pTree = do
    pBranch <|> pEmpty

pBranch = do
    x <- letter
    l <- pTree
    r <- pTree
    return $ Branch x l r

pEmpty = do
    char '.'
    return Empty

ds2tree str = 
    case parse pTree "" str of
        Right t -> t
        Left e -> error (show e)

tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds (Branch x l r) = x : (tree2ds l ++ tree2ds r)

main = do
    let value = "abd..e..c.fg..."
    print $ ds2tree value
    let value2 = Branch 'x' (Branch 'y' Empty Empty) (Branch 'z' (Branch '0' Empty Empty) Empty)
    print $ tree2ds value2