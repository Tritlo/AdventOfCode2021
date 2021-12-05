{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Text.ParserCombinators.ReadP
import Data.Maybe (mapMaybe, isJust)
import Data.Either
import Data.List (sort, partition, group)
import Debug.Trace (traceShowId)

data Point = Point (Int, Int) deriving (Eq, Ord)
data Line = Line Point Point

instance Read Line where
    readsPrec n = readP_to_S parseLine


instance Show Line where
    show (Line p1 p2) = show p1 ++ " -> " ++ show p2

parsePoint :: ReadP Point
parsePoint = do x <- readPInt
                char ','
                y <- readPInt
                return $ Point (x,y)

instance Read Point where
    readsPrec n = readP_to_S parsePoint
instance Show Point where
    show (Point (x,y)) = show x ++ "," ++ show y

parseLine :: ReadP Line
parseLine = do p1 <- parsePoint
               string " -> "
               Line p1 <$> parsePoint


readPInt :: ReadP Int
readPInt = readS_to_P reads

readInput :: FilePath -> IO [Line]
readInput = fmap (map (normX . read @Line) . lines) . readFile

normX :: Line -> Line
normX l@(Line p1@(Point (x1,y1)) p2@(Point (x2, y2))) | x1 <= x2 = l
                                                      | otherwise = Line p2 p1

vOrH :: Line -> Maybe (Either Line Line)
vOrH l@(Line (Point (x1,y1)) (Point (x2,y2)))
 | x1 == x2  = Just (Left l)
 | y1 == y2 = Just (Right l)
 | otherwise = Nothing

on :: Point -> Line -> Bool
on (Point (x,y)) (Line (Point (x1,y1)) (Point (x2,y2)))
   | x1 == x2 && x1 == x = min y1 y2 <= y &&  y <= max y1 y2
   | y1 == y2 && y1 == y = min x1 x2 <= x &&  x <= max x1 x2
   | otherwise = False

onDiag:: Point -> Line -> Bool
onDiag p@(Point (x,y)) l@(Line (Point (x1,y1)) (Point (x2,y2)))
    | y1 == y2 || x1 == x2 = on p l
    | min x1 x2 <= x && x <= max x1 x2 &&
      min y1 y2 <= y && y <= max y1 y2
      = y == (x-x1)*((y2-y1)`div`(x2-x1)) + y1
    | otherwise = False


toPoints :: Line -> [Point]
toPoints (Line p1 p2) = [p1,p2]

xCoord :: Point -> Int
xCoord (Point (x,_)) = x
yCoord :: Point -> Int
yCoord (Point (_,y)) = y

space :: [Line] -> Point
space lines = Point (maximum $ map xCoord allPoints, maximum $ map yCoord allPoints)
  where allPoints = concatMap toPoints lines

allPointsInSpace :: Point -> [Point]
allPointsInSpace (Point (x,y)) = [Point (a,b) | b <- [0..y], a <- [0..x] ]

numLinesOn :: [Line] -> Point -> Int
numLinesOn lns p = length (filter (on p) lns)

numLinesOnDiag :: [Line] -> Point -> Int
numLinesOnDiag lns p = length (filter (onDiag p) lns)

onAny :: [Line] -> Point -> Bool
onAny [] _ = False
onAny (l:lns) p | onDiag p l = True
                | otherwise = onAny lns p


printMap :: [Line] -> IO ()
printMap lns = mapM_ pl [0..py]
 where Point (px,py) = space lns
       pl :: Int -> IO ()
       pl y = mapM_ (printPoint y) [0..px] >> putStr "\n"
       printPoint :: Int -> Int -> IO ()
       printPoint y x = putStr out
        where nlo = numLinesOn lns (Point (x,y))
              out = if nlo == 0
                    then "."
                    else show nlo

task1 :: IO ()
task1 = do lns <- map (either id id ) . mapMaybe vOrH <$> readInput "input"
           let sp = space lns
               aps = allPointsInSpace sp
           print $ length (filter (>= 2) $ map (numLinesOn lns) aps)

lPoints :: Line -> [Point]
lPoints l@(Line (Point (x1,y1)) (Point (x2,y2)))
 | y1 == y2 = map (Point . (,y1)) [sx..bx]
 | x1 == x2 = map (Point . (x1,)) [sy..by]
 | otherwise = zipWith (curry Point) xDiag yDiag
  where sx = min x1 x2
        bx = max x1 x2
        sy = min y1 y2
        by = max y1 y2
        yDiag = if y1 < y2
                then [y1..y2]
                else reverse [y2..y1]
        xDiag = [x1..x2]

task2 :: IO ()
task2 = do lns <- readInput "input"
           let pointsOnLines = concatMap lPoints lns
               gps :: [[Point]]
               gps = group $ sort pointsOnLines
           -- 233 ms
           -- print $ length pointsOnLines
           print $ length $ filter (>=2) $ map length gps
           -- 14 seconds
           --print $ length (filter (>= 2) $ map (numLinesOnDiag lns) $ allPointsInSpace $ space lns)

main :: IO ()
main = task2

