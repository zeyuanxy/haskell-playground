import Data.List (tails)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = error "not enough elements"
combinations n xs@(x:xs') = if length xs == n then [xs]
                            else [x:ys | ys <- combinations (n - 1) xs'] ++ (combinations n xs')

main :: IO ()
main = do
    let value = combinations 3 "abcdef"
    print value
