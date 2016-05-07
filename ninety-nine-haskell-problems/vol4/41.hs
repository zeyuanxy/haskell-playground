import Data.Maybe

isPrime :: Int -> Bool
isPrime n | n <= 1 = False
          | n == 2 = True
          | otherwise = all (\x -> n `mod` x /= 0) [2 .. s]
            where s = floor . sqrt . fromIntegral $ n 

primesR :: Int -> Int -> [Int]
primesR x y = filter isPrime [x..y]

goldbach :: Int -> (Int, Int)
goldbach n = head [(x, y) | x <- primesR 2 n, y <- primesR 2 n, x + y == n]

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

goldbach' :: Int -> Int -> Maybe (Int, Int)
goldbach' n z = head' [(x, y) | x <- primesR 2 n, y <- primesR 2 n, x + y == n, x > z, y > z]

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList x y = map goldbach [even_x, even_x + 2..y]
    where even_x = max ((x + 1) `div` 2 * 2) 4

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' x y z = map fromJust . filter isJust . map (\x -> goldbach' x z) $ [even_x, even_x + 2..y]
    where even_x = max ((x + 1) `div` 2 * 2) 4

main :: IO ()
main = do
    x <- getLine
    y <- getLine
    -- let value = goldbachList (read x) (read y)
    -- print value
    z <- getLine
    let value' = goldbachList' (read x) (read y) (read z)
    print value'
