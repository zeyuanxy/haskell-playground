import Data.List
import System.Random

rnd_select :: (Eq a) => [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return . take n . nub $ [xs !! i | i <- randomRs (0, (length xs) - 1) gen]

rnd_permu :: (Eq a) => [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)

main :: IO ()
main = do
    value <- rnd_permu "abcdef"
    print value
