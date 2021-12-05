module Day3 where
import Data.List (transpose)
import Debug.Trace (traceShowId)


readInput :: FilePath -> IO [[Int]]
readInput = fmap (map readNum . lines) . readFile

readNum :: String -> [Int]
readNum = map readOne
 where readOne '0' = 0
       readOne _ = 1

binToInt :: [Bool] -> Int
binToInt =  sum . map ((2^) .fst) . filter snd . zip [0..] . reverse


task1 :: [[Int]] -> Int
task1 inp = binToInt (map gamma tinp) * binToInt (map eps tinp)
  where tinp = transpose inp
        gamma digit = sum digit > (length digit) `div`2
        eps digit = sum digit <= (length digit) `div`2



task2 :: [[Int]] -> Int
task2 inp@(n:_)  = product $ map (binToInt . map intToBool) [task2Mc inp inds, task2Lc inp inds]
  where
      inds = [0.. (length n - 1)]
      intToBool 1 = True
      intToBool 0 = False
      task2Mc [lastOne] _ = lastOne
      task2Mc curInp (i:is) = task2Mc newInp is
        where curBits = map (!! i) curInp
              num1 = sum curBits
              num0 = length curBits - num1
              mostCommon = if num1 >= num0 then 1 else 0
              newInp = filter (\el -> (el!!i) == mostCommon) curInp

      task2Lc [lastOne] _ = lastOne
      task2Lc curInp (i:is) = task2Lc newInp is
        where curBits = map (!! i) curInp
              num1 = sum curBits
              num0 = length curBits - num1
              leastCommon = if num0 <= num1 then 0 else 1
              newInp = filter (\el -> (el!!i) == leastCommon) curInp

