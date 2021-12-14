module Main where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sort, group)
import Debug.Trace (traceShow)


readInput :: FilePath -> IO (String, Map Char (Map Char Char))
readInput fp = do (template:_:rules) <- lines <$> readFile fp
                  let ruleMaps = Map.unionsWith Map.union $ map mkRuleMap rules
                  return (template, ruleMaps)
    where mkRuleMap :: String -> Map Char (Map Char Char)
          mkRuleMap (c1:c2:_:_:_:_:c3:_) = Map.singleton c1 (Map.singleton c2 c3)

toPairs :: String -> [(Char, Char)]
toPairs (a:b:r) = (a,b):toPairs (b:r)
toPairs _ = []

fromPairs :: [(Char, Char)] -> String
fromPairs [] = []
fromPairs [(a,b)] = [a,b]
fromPairs ((a,b):r) = a:fromPairs r

type Rules = Map Char (Map Char Char)
applyRule :: Rules -> (Char, Char) -> [(Char, Char)]
applyRule rules (a,b) | Just af <- rules Map.!? a,
                        Just n <- af Map.!? b = [(a,n), (n,b)]
                      | otherwise = [(a,b)]

applyRules :: Rules -> [(Char, Char)] -> [(Char, Char)]
applyRules rules = concatMap (applyRule rules)


nsteps :: Int -> Map Char (Map Char Char) -> [(Char, Char)] -> [(Char, Char)]
nsteps 0 rules pairs = pairs
nsteps n rules pairs = nsteps (n-1) rules $ traceShow (n, length res) res
 where res = applyRules rules pairs

task1 template rules = maxl - minl
    where after = fromPairs $ nsteps 10 rules $ toPairs template
          lengths = map length $ group $ sort after
          minl = minimum lengths
          maxl = maximum lengths

task2 :: [Char] -> Map Char (Map Char Char) -> Int
task2 template rules = maxl - minl
    where after = fromPairs $ nsteps 40 rules $ toPairs template
          lengths = map length $ group $ sort after
          minl = minimum lengths
          maxl = maximum lengths

type MemoMap = Map ((Char,Char), Int) (Map Char Int)
task2v3 :: [Char] -> Rules -> Int
task2v3 template rules = maxc - minc
    where pairs = toPairs template
          templCount = Map.fromList $ map (\l@(x:_) -> (x,length l)) $ group $ sort template
          chars = Map.unionsWith (+) $ map fst run
          run = map (task2' 40  Map.empty) pairs
          final = Map.unionWith (+) templCount chars
          minc = minimum final
          maxc = maximum final

          task2' :: Int ->  MemoMap -> (Char, Char) -> (Map Char Int, MemoMap )
          task2' 1 memo p | Just res <- memo Map.!? (p,1) = (res, memo)
                          | [(_,c),(_,_)] <- applyRule rules p,
                            mr <- Map.fromList $ zip [c] $ repeat 1
                            = (mr, Map.insert (p,1) mr memo)
                          | otherwise = (Map.empty, Map.insert (p,0) Map.empty memo)
          task2' n memo p@(a,b) | Just res <- memo Map.!? (p,n) = (res, memo)
                                | [p1@(_,c),p2] <- applyRule rules p ,
                                  (m1, m') <- task2' (n-1) memo p1,
                                  (m2, m'') <- task2' (n-1) m' p2,
                                  mr <- Map.unionsWith (+) [m1,m2,Map.singleton c 1]
                                = (mr, Map.insert (p,n) mr m'')
                                | otherwise = (Map.empty, Map.insert (p,n) Map.empty memo)



main :: IO ()
main = do (template, rules) <- readInput "input"
          print $ task2v3 template rules
