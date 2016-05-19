-- @Author: Zeyuan Shang
-- @Date:   2016-05-19 21:51:58
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-19 22:26:12
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

treeToPreorder :: Tree Char -> String
treeToPreorder Empty = ""
treeToPreorder (Branch x l r) = x : treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> String
treeToInorder Empty = ""
treeToInorder (Branch x l r) = treeToInorder l ++ x : treeToInorder r

preInTree :: Monad m => String -> String -> m (Tree Char)
preInTree [] [] = return Empty
preInTree po@(x:xs) io = do
	let (lio, _:rio) = break (== x) io
	let (lpo, rpo) = splitAt (length lio) xs
	l <- preInTree lpo lio
	r <- preInTree rpo rio
	return $ Branch x l r
preInTree _ _ = fail "woops"

main = do
    let t = stringToTree "a(b(d,e),c(,f(g,)))"
    let po = treeToPreorder t
    let io = treeToInorder t
    preInTree po io >>= print