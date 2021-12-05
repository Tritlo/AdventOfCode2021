{-# LANGUAGE TypeApplications #-}
module Main where
import Text.ParserCombinators.ReadP
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List ( transpose, partition )
import Data.Maybe (mapMaybe, isJust, isNothing)


newtype Board = Board [Set Int] deriving (Show)

data Input = Input [Int] [Board] deriving (Show)

instance Read Input where
  readsPrec n = readP_to_S $ Input <$> withSkip (parseNumsCalled n)
                                   <*> many (withSkip parseBoard)

withSkip :: ReadP b -> ReadP b
withSkip p = p >>= (\r -> skipSpaces >> return r)

boardSets :: [[Int]] -> [Set Int]
boardSets els = map Set.fromList els ++ map Set.fromList (transpose els)

mbWinBingo :: Set Int -> Board -> Maybe Int
mbWinBingo soFar (Board sets) =
    if any allCalled sets
    then Just (sum $ Set.toList (Set.unions sets Set.\\ soFar))
    else Nothing
  where allCalled = flip Set.isSubsetOf soFar

parseBoard :: ReadP Board
parseBoard = Board . boardSets <$> count 5 (withSkip parseLine)
  where parseLine :: ReadP [Int]
        parseLine = count 5 (withSkip (readS_to_P (readsPrec 10)))

parseNumsCalled :: Int -> ReadP [Int]
parseNumsCalled n = sepBy (readS_to_P $ readsPrec n) (char ',')

task1 :: FilePath -> IO Int
task1 inp =
     do Input nums boards <- read @Input <$> readFile inp
        return $ task1' Set.empty boards nums

task1' :: Set Int -> [Board] -> [Int] -> Int
task1' soFar boards [] = error "no winner ever!"
task1' numsSoFar boards (n:ns) =
        case mapMaybe (mbWinBingo nsf') boards of
            (winner:_) -> winner*n
            _ -> task1' nsf' boards ns
        where nsf' = Set.insert n numsSoFar

task2 :: FilePath -> IO Int
task2 inp =
     do Input nums boards <- read @Input <$> readFile inp
        return $ task2' Set.empty boards nums
  where task2' soFar boards [] = error "no winner ever!"
        task2' soFar bs@[board] nums = task1' soFar bs nums
        task2' numsSoFar boards (n:ns) = task2' nsf' notYet ns
            where nsf' = Set.insert n numsSoFar
                  notYet = filter (isNothing . mbWinBingo nsf') boards

main :: IO ()
main = do
  task1 "input-day4" >>= print
  task2 "input-day4" >>= print