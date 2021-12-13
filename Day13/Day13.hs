{-# LANGUAGE TypeApplications #-}
module Main where

import Text.ParserCombinators.ReadP
import qualified Data.Set as Set
import Data.List (sort)

type Point = (Int, Int)

data FoldInstr = X Int | Y Int deriving (Show)

instance Read FoldInstr where
    readsPrec _ = readP_to_S parse
      where parse :: ReadP FoldInstr
            parse = do _ <- string "fold along "
                       x <- choice [ string "x=" >> X <$> parseInt
                                   , string "y=" >> Y <$> parseInt ]
                       skipSpaces
                       return x


parseInt :: ReadP Int
parseInt = readS_to_P reads

data Input = Input [Point] [FoldInstr] deriving (Show)

instance Read Input where
    readsPrec _ = readP_to_S parse
      where parse :: ReadP Input
            parse = do points <- many1 parsePoint
                       skipSpaces
                       folds <- many1 (readS_to_P reads)
                       return $ Input points folds

parsePoint :: ReadP (Int, Int)
parsePoint = do x <- parseInt
                char ','
                y <- parseInt
                skipSpaces
                return (x,y)

readInput :: FilePath -> IO Input
readInput = fmap read . readFile

applyInstr :: FoldInstr -> [Point] -> [Point]
applyInstr (X n) ps = map (mirror n) xs `zip` ys
    where (xs,ys) = unzip ps
applyInstr (Y n) ps = xs `zip` map (mirror n) ys
    where (xs,ys) = unzip ps

mirror :: Int -> Int -> Int
mirror n x | x <= n = x
mirror n x = 2*n - x

printPoints :: [Point] -> String
printPoints ps = unlines $
                 [[ if (x,y) `Set.member` pSet
                    then '#' else '.' | x <- [0..xMax]]| y <- [0..yMax]]
  where yMax = maximum $ map snd ps
        xMax = maximum $ map fst ps
        pSet = Set.fromList ps


main :: IO ()
main = do Input ps fs@(f:_) <- readInput "input"
          print fs
          putStrLn (printPoints $ foldl (flip applyInstr) ps fs)