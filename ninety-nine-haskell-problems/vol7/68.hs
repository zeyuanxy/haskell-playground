-- @Author: Zeyuan Shang
-- @Date:   2016-05-19 21:51:58
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-19 22:05:54
import Text.Parsec.String
import Text.Parsec hiding (Empty)

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show, Eq)

pTree :: Parser (Tree Char)
pTree = pBranch <|> pEmpty

pBranch = do
    x <- letter
    do 
        char '('
        l <- pTree
        char ','
        r <- pTree
        char ')'
        return $ Branch x l r
        <|> return (Branch x Empty Empty)

pEmpty = return Empty

stringToTree str = 
    case parse pTree "" str of
        Right t -> t
        Left e -> error (show e)

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = x : '(' : treeToString l ++ "," ++ treeToString r ++ ")"

main = do
    let tree = stringToTree "x(y,a(,b))"
    let string = treeToString tree
    print tree
    print string