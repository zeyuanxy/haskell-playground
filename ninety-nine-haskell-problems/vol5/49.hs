-- @Author: Zeyuan Shang
-- @Date:   2016-05-09 23:02:29
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-09 23:08:22

gray :: Int -> [String]
gray 0 = [""]
gray n = let xs = gray (n - 1) in map ('0':) xs ++ map ('1':) (reverse xs)

main = do
    let value = gray 3
    print value