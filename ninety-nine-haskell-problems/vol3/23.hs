import Data.List
import System.Random

rnd_select :: (Eq a) => [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return . take n . nub $ [xs !! i | i <- randomRs (0, (length xs) - 1) gen]

main :: IO ()
main = do
    value <- rnd_select "abcdefgh" 3
    print value
