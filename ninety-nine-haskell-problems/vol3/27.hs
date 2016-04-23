import Data.List (tails)

combinations :: Int -> [a] -> [([a], [a])]
combinations 0 xs = [([], xs)]
combinations _ [] = error "not enough elements"
combinations n xs@(x:xs') = if length xs == n then [(xs, [])]
                            else [(x:ys, rest) | (ys, rest) <- combinations (n - 1) xs'] ++ [(ys', x:rest') | (ys', rest') <- combinations n xs']

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs = [g:gs | (g, rest) <- combinations n xs, gs <- group ns rest]

main :: IO ()
main = do
    let value = group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
    print value
