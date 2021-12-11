module Main where
import Data.Maybe (mapMaybe)
import Data.List (sort)


readInput :: FilePath -> IO [String]
readInput = fmap lines . readFile

matches :: Char -> Maybe Char
matches '(' = Just ')'
matches '[' = Just ']'
matches '{' = Just '}'
matches '<' = Just '>'
matches _ = Nothing

score1 :: Char -> Maybe Int
score1 ')' = Just 3
score1 ']' = Just 57
score1 '}' = Just 1197
score1 '>' = Just 25137
score1 _ = Nothing

task1 :: [Char] -> Maybe Int
task1 = checkLine' []
  where checkLine' _ [] = Nothing
        checkLine' stack (c:cs) | Just n <- matches c = checkLine' (n:stack) cs
                                | (s:rs) <- stack,
                                  s == c = checkLine' rs cs
                                | otherwise = score1 c

task2P :: [Char] -> Maybe String
task2P = checkLine' []
  where checkLine' stack [] = Just stack
        checkLine' stack (c:cs) | Just n <- matches c = checkLine' (n:stack) cs
                                | (s:rs) <- stack,
                                  s == c = checkLine' rs cs
                                | otherwise = Nothing

score2 :: String -> Int
score2 = score2' 0
  where score2' cur [] = cur
        score2' cur (c:cs) = score2' ((cur*5) + sc c) cs
        sc ')' = 1
        sc ']' = 2
        sc '}' = 3
        sc '>' = 4
        sc _ = error "invalid character!"

task2 :: [String] -> Int
task2 lns = (sort scores) !! (length scores `div` 2)
    where
        completes :: [String]
        completes = mapMaybe task2P lns
        scores = map score2 completes



main :: IO ()
main = readInput "input" >>= print . task2