module Main where

import Data.List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (mapMaybe)
import Data.Function (on)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (traceShow, traceShowId)

readInput1 :: FilePath -> IO [Int]
readInput1 = fmap (map f . lines) . readFile
  where f :: String -> Int
        f = countUniques . IM.fromList . map (\l@(x:_) -> (x, length l))
            . group . sort . map length . words . tail . dropWhile (/= '|')
        countUniques :: IntMap Int -> Int
        countUniques im = sum $ mapMaybe (im IM.!?) [2, 3, 4, 7]


readInput2 :: FilePath -> IO [([Set Char], [Set Char])]
readInput2 = fmap (map f2 . lines) . readFile
  where f2 ln = (sortBy (compare `on` length) $ map Set.fromList $ words segs
                , map Set.fromList $ words rest)
         where (segs, '|':' ':rest) = span (/= '|') ln


findNums :: [Set Char] -> Map (Set Char) Int
findNums (one:seven:four:rest) =
        Map.fromList $ zip [zero,one,two, three, four,five,six,seven,eight, nine] [0..]
  where [x,y,z] = filter ((6 ==) . Set.size ) rest
        (six, [x',y'])  | Set.size (x Set.\\ one) == 5 = (x, [y,z])
                        | Set.size (y Set.\\ one) == 5 = (y, [x,z])
                        | otherwise = (z, [x,y])
        (zero, nine) = if Set.size (x' Set.\\ four) == 2
                       then (y',x') else (x',y')
        eight = last rest
        a = eight Set.\\ six
        b = one Set.\\ a
        f = eight Set.\\ zero
        five = nine Set.\\ a
        efc = five Set.\\ seven
        c = (nine Set.\\ seven) Set.\\ four
        e = efc Set.\\ f Set.\\ c
        three = nine Set.\\ e
        two = (eight Set.\\ b) Set.\\ e

--  dddd
-- e    a
-- e    a
--  ffff
-- g    b
-- g    b
--  cccc
task2 :: [([Set Char], [Set Char])] -> Int
task2 = sum . map (uncurry task2')
  where task2' key digs = tho*1000 + hu*100 + te*10 + u
          where decode = findNums key
                [tho,hu,te,u] = map (decode Map.!) digs

main :: IO ()
main = readInput2 "input" >>= (print . task2)
