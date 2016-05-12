-- @Author: Zeyuan Shang
-- @Date:   2016-05-12 22:01:53
-- @Last Modified by:   Zeyuan Shang
-- @Last Modified time: 2016-05-12 22:15:00
import Data.List
import Data.Ord (comparing)


data HuffmanTree a = Leaf a | Branch (HuffmanTree a) (HuffmanTree a)

toString (Branch l r) = [(x, '0':code) | (x, code) <- toString l] ++
                        [(x, '1':code) | (x, code) <- toString r]
toString (Leaf x) = [(x, "")]

huffman :: (Ord a, Ord w, Num w) => [(a, w)] -> [(a, String)]
huffman xs = sortBy (comparing fst) . toString . construct . sortBy (comparing fst) $ [(w, Leaf a) | (a, w) <- xs] 
    where construct [(_, t)] = t
          construct ((w1, t1):(w2, t2):wts) = construct $ insertBy (comparing fst) (w1 + w2, Branch t1 t2) wts

main = do
    let value = huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
    print value
          