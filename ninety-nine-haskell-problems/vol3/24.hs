import System.Random
import Data.List

diff_select :: Int -> Int -> IO [Int]
diff_select n m = do 
    gen <- getStdGen
    return . take n . nub $ randomRs (1, m) gen

main :: IO ()
main = do
    value <- diff_select 6 49
    print value
