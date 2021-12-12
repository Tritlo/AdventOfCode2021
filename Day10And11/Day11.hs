module Main where


import qualified  Data.Map as Map
import Data.Map (Map)
import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (sortBy, groupBy, group, sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.Char

readInput :: FilePath -> IO [[Int]]
readInput = fmap (map (map digitToInt) . lines) . readFile


printSt' :: (Int, Int) -> Map (Int,Int) Int -> String
printSt' (xr,yr) mp  = unlines $ map toLine [0..yr]
    where toLine l = map (toDig . (mp Map.!)) [(x,l) | x <- [0..xr]]
          toDig :: Int -> Char
          toDig i = intToDigit i

--task1 :: [[Int]] -> Map Int (Set (Int, Int))
task1' inpt = sim 100 (0, Map.fromList yindexed)
    where
        dim = ((length $ head inpt)-1, (length inpt)-1)
        sim 0 (x,a) = trace (printSt' dim a) x
        sim n c@(_,a) = sim (n-1) $ trace (printSt' dim a) step c
        step (c, curMp) = (c+nf, nmp)
          where (nf, nmp) = flash flashing inc1
                inc1 = Map.map (+1) curMp
                flashing = Map.keys $ Map.filter (== 10) inc1
                clearFlashed = Map.map (\x -> if x > 9 then 0 else x)
                flash [] mp = (Map.size (Map.filter (>9) mp), clearFlashed mp)
                flash flashing mp = flash flashing' nmap
                    where
                        flashedBefore = Set.fromList $ Map.keys (Map.filter (> 9) mp)
                        nflash = map (\l@(x:_) -> (x,length l)) $ group $ sort $ concatMap area flashing
                        nmap = foldr (\(c,n) m -> Map.update (Just . (+n)) c m) mp nflash
                        flashing' = filter (not . flip Set.member flashedBefore)
                                         $ Map.keys $ Map.filter (> 9) nmap
        area :: (Int, Int) -> [(Int,Int)]
        area c@(x,y) = filter inRange [(x+i,y+j) | i<- [-1,0,1], j <- [-1,0,1]]
            where inRange (x,y) = x >= 0 && y >= 0 && x < 10 && y < 10




        xindexed :: [[(Int,Int)]]
        xindexed = map (zip [0..]) inpt
        yindexed :: [((Int,Int), Int)]
        yindexed = concat $ zipWith f [0..] xindexed
          where f :: Int -> [(Int,Int)] -> [((Int,Int), Int)]
                f y arr = map (\(x,v) -> ((x,y),v)) arr


task2' inpt = simUntilFlash 0 $ Map.fromList yindexed
    where
        dim = ((length $ head inpt)-1, (length inpt)-1)
        simUntilFlash n mp = if flashed == 100
                             then trace (printSt' (9,9) mp') (n+1)
                             else simUntilFlash (n+1) $ trace (printSt' (9,9) mp) mp'
          where (flashed, mp') = step mp

        step curMp = (nf, nmp)
          where (nf, nmp) = flash flashing inc1
                inc1 = Map.map (+1) curMp
                flashing = Map.keys $ Map.filter (== 10) inc1
                clearFlashed = Map.map (\x -> if x > 9 then 0 else x)
                flash [] mp = (Map.size (Map.filter (>9) mp), clearFlashed mp)
                flash flashing mp = flash flashing' nmap
                    where
                        flashedBefore = Set.fromList $ Map.keys (Map.filter (> 9) mp)
                        nflash = map (\l@(x:_) -> (x,length l)) $ group $ sort $ concatMap area flashing
                        nmap = foldr (\(c,n) m -> Map.update (Just . (+n)) c m) mp nflash
                        flashing' = filter (not . flip Set.member flashedBefore)
                                         $ Map.keys $ Map.filter (> 9) nmap
        area :: (Int, Int) -> [(Int,Int)]
        area c@(x,y) = filter inRange [(x+i,y+j) | i<- [-1,0,1], j <- [-1,0,1]]
            where inRange (x,y) = x >= 0 && y >= 0 && x < 10 && y < 10




        xindexed :: [[(Int,Int)]]
        xindexed = map (zip [0..]) inpt
        yindexed :: [((Int,Int), Int)]
        yindexed = concat $ zipWith f [0..] xindexed
          where f :: Int -> [(Int,Int)] -> [((Int,Int), Int)]
                f y arr = map (\(x,v) -> ((x,y),v)) arr
main :: IO ()
main = readInput "example-day11" >>= print . task2'