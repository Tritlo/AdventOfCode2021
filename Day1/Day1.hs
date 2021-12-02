{-# LANGUAGE TypeApplications #-}
module Main where

import Debug.Trace


readInput :: FilePath -> IO [Int]
readInput file = map (read @Int) . lines <$> readFile file

example :: IO [Int]
example = readInput "example"

-- First one!
countInc :: [Int] -> Int
countInc (first:rest) = countInc' 0 first rest
 where countInc' :: Int -> Int -> [Int] -> Int
       countInc' cur _ [] = cur
       countInc' cur prevMes (curMes:rest) = countInc' cur' curMes rest
        where cur' | prevMes < curMes = cur + 1
                   | otherwise = cur

-- Second one!
countInc2 :: [Int] -> Int
countInc2 (a:b:c:rest) = countInc' 0 [a,b,c] rest
 where countInc' :: Int -> [Int] -> [Int] -> Int
       countInc' cur _ [] = cur
       countInc' cur [a,b,c] (d:rest) = countInc' cur' [b,c,d] rest
        where cur' | a < d = cur + 1
                   | otherwise = cur

countInc2 _ = error "too few elements!"

main :: IO ()
main = readInput "input" >>= print . countInc2


