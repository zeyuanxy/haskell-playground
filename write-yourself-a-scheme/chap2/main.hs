-- @Author: Zeyuan Shang
-- @Date:   2016-08-13 14:00:45
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-08-13 14:04:22

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)