-- @Author: Zeyuan Shang
-- @Date:   2016-06-14 23:08:34
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-06-14 23:12:25
import Data.Char
import Data.List

fullWords :: Int -> String
fullWords x = concat $ intersperse "-" [digits !! (digitToInt d)  | d <- show x]
    where digits = ["zero", "one", "two", "three", "four",
                    "five", "six", "seven", "eight", "nine"]

main = do 
    let value = fullWords 175
    print value