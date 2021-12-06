module Main where
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

example :: [Int]
example = [3,4,3,1,2]

input :: [Int]
input = [3,5,3,5,1,3,1,1,5,5,1,1,1,2,2,2,3,1,1,5,1,1,5,5,3,2,2,5,4,4,1,5,1,4,4,5
        ,2,4,1,1,5,3,1,1,4,1,1,1,1,4,1,1, 1,1,2,1,1,4,1,1,1,2,3,5,5,1,1,3,1,4,1
        ,3,4,5,1,4,5,1,1,4,1,3,1,5,1,2,1,1,2,1,4,1,1,1,4,4,3,1,1,1,1,1,4,1,4,5,2
        ,1,4,5,4,1,1,1,2,2,1,4,4,1,1,4,1,1,1,2,3,4,2,4,1,1,5,4,2,1,5,1,1,5,1,2,1
        ,1,1,5,5,2,1,4,3,1,2,2,4,1,2,1,1,5,1,3,2,4,3,1,4,3,1,2,1,1,1,1,1,4,3,3,1
        ,3,1,1,5,1,1,1,1,3,3,1,3,5,1,5,5,2,1,2,1,4,2,3,4,1,4,2,4,2,5,3,4,3,5,1,2
        ,1,1,4,1,3,5,1,4,1,2,4,3,1,5,1,1,2,2,4,2,3,1,1,1,5,2,1,4,1,1,1,4,1,3,3,2
        ,4,1,4,2,5,1,5,2,1,4,1,3,1,2,5,5,4,1,2,3,3,2,2,1,3,3,1,4,4,1,1,4,1,1,5,1
        ,2,4,2,1,4,1,1,4,3,5,1,2,1]


type Memo = Map (Int, Int) Integer
lanternFish :: Int -> Int -> Integer
lanternFish days i = snd $ lanternFish' Map.empty days i

lanternFish' :: Memo -> Int -> Int -> (Memo, Integer)
lanternFish' memo 0 _ = (memo, 1)
lanternFish' memo days 0
 = case memo Map.!? (days,0)  of
     Just res -> (memo, res)
     _ -> let (memo', r) = lanternFish' memo (days - 1) 6
              (memo'', r2) = lanternFish' memo' (days - 1) 8
          in (Map.insert (days,0) (r+r2) memo'', r+r2)
lanternFish' memo days timer = lanternFish' memo (days - 1) (timer - 1)

lanternPool :: [Int] -> Int -> Integer
lanternPool inits days = sum $ map numPerPool iAndLs
  where grouped = group $ sort inits
        iAndLs = map (\l@(x:_) -> (x, length l)) grouped
        numPerPool :: (Int, Int) -> Integer
        numPerPool (i, l) = lanternFish days i*fromIntegral l

-- We can also use past memoization in the new one!
lanternPoolM :: [Int] -> Int -> Integer
lanternPoolM inits days = snd $ foldr numPerPool (Map.empty,0) iAndLs
  where grouped = group $ sort inits
        iAndLs = map (\l@(x:_) -> (x, length l)) grouped
        numPerPool :: (Int, Int) -> (Memo, Integer) -> (Memo, Integer)
        numPerPool (i, l) (memo, cur) = (memo', cur + fromIntegral l*r)
            where (memo', r) = lanternFish' memo days i



main :: IO ()
main = print (lanternPoolM input 256)