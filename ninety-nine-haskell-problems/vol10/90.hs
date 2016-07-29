-- @Author: Zeyuan Shang
-- @Date:   2016-07-29 20:02:02
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-07-29 20:20:13

queens :: Int -> [[Int]]
queens n = placeQueens n
    where placeQueens 0 = [[]]
          placeQueens k = [col : qs | qs <- placeQueens (k - 1), col <- [1..n], isSafe col qs]

isSafe :: Int -> [Int] -> Bool
isSafe col qs = all (\(r, c) -> col /= c && abs (col - c) /= (n + 1 - r)) (zip [n, n - 1..1] qs)
    where n = length qs

main = do
    print . length $ queens 8