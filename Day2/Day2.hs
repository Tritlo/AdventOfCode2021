{-# LANGUAGE TypeApplications #-}
module Main where

import Text.ParserCombinators.ReadP
    ( choice, readP_to_S, readS_to_P, skipSpaces, ReadP )
import qualified Text.ParserCombinators.ReadP as Parse
import Data.Maybe ( mapMaybe )
import System.FilePath (isAbsolute)

example :: IO [String]
example = lines <$> readFile "example"

data Instruction = Forward Int
                 | Down Int
                 | Up Int
                 deriving (Eq, Ord, Show)


instance Read Instruction where
    readsPrec = readP_to_S . instrParser

instrParser :: Int -> ReadP Instruction
instrParser _ = choice $
     map parseOne ["forward", "up", "down"]

parseOne :: String -> ReadP Instruction
parseOne str = do _ <- Parse.string str
                  skipSpaces
                  num <- readS_to_P (readsPrec @Int 5)
                  return $ case str of
                              "forward" -> Forward num
                              "up" -> Up num
                              "down" -> Down num

fromForward :: Instruction -> Maybe Int
fromForward (Forward n) = Just n
fromForward _ = Nothing

fromUp :: Instruction -> Maybe Int
fromUp (Up n) = Just n
fromUp _ = Nothing

fromDown :: Instruction -> Maybe Int
fromDown (Down n) = Just n
fromDown _ = Nothing

instrs :: FilePath -> IO [Instruction]
instrs = fmap (map (read @Instruction) . lines) . readFile


task1 :: FilePath -> IO Int
task1 fp = do is <- instrs fp
              let fors  = mapMaybe fromForward is
                  ups   = mapMaybe fromUp is
                  downs = mapMaybe fromDown is
                  horiz = sum fors
                  depth = sum downs - sum ups
              return (horiz*depth)

task2 :: FilePath -> IO Int
task2 fp = do is <- instrs fp
              return $ process 0 0 0 is
  where process :: Int -> Int -> Int -> [Instruction] -> Int
        process depth aim horiz [] = depth*horiz
        process depth aim horiz (Up   n:res) =
            process depth (aim-n) horiz res
        process depth aim horiz (Down n:res) =
            process depth (aim+n) horiz res
        process depth aim horiz (Forward n:res) =
            process (depth +(aim*n)) aim (horiz+n) res





main :: IO ()
main = do task2 "input" >>= print