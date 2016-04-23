import Data.List
import Data.Ord (comparing)

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)

lfsort :: [[a]] -> [[a]]
lfsort xs = concat groups
    where groups = lsort . groupBy (\xs ys -> length xs == length ys) $ lsort xs

main :: IO ()
main = do
    let value = lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]    
    print value
