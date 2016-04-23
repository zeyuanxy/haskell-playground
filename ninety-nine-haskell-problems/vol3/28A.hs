import Data.List
import Data.Ord (comparing)

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)

lsort' :: [[a]] -> [[a]]
lsort' = sortBy (\xs ys -> compare (length xs) (length ys))

main :: IO ()
main = do
    let value = lsort ["abc","de","fgh","de","ijkl","mn","o"]
    print value
    let value' = lsort' ["abc","de","fgh","de","ijkl","mn","o"]
    print value'
