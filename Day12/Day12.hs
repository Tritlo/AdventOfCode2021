module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char ( isAsciiUpper )
import Debug.Trace (traceShowId, traceShow)

-- import Data.Graph (Graph, Vertex)
-- import qualified Data.Graph as Graph

-- type GNode = (String, String, [String])

-- readInput :: FilePath -> IO (Graph, Vertex -> GNode, String -> Maybe Vertex)
-- readInput = fmap (Graph.graphFromEdges . map toNode . lines) . readFile
--   where
--       toNode :: String -> GNode
--       toNode = undefined

readInput :: FilePath -> IO (Map String (Set String))
readInput = fmap (parse . lines) . readFile
  where parse lns = Map.unionsWith Set.union $ map ( pairToMap . fmap (drop 1) . span (/= '-')) lns
        pairToMap (a,b) = Map.fromList [(a,Set.singleton b), (b, Set.singleton a)]

isBig :: String -> Bool
isBig = any isAsciiUpper


task1 :: Map String (Set String) -> [[String]]
task1 mp = numPaths' (Set.singleton "start") "start"
  where numPaths' bigVisited "end" = [["end"]]
        numPaths' smallVisited k = map (k:) nextPaths
            where
                nextPaths = concatMap (numPaths' smallVisited') next
                next = Set.toList ((mp Map.! k) Set.\\ smallVisited)
                smallVisited' = if isBig k
                                then smallVisited
                                else Set.insert k smallVisited

task2 :: Map String (Set String) -> [[String]]
task2 mp = (Set.toList . Set.fromList) $ concatMap (\tk -> numPaths' tk (Map.fromList [("start", 1), (tk,-1)])  "start") smallKeys
  where smallKeys = filter (\k -> k /= "start" && k /= "end" && not (isBig k)) $ Map.keys mp
        numPaths' :: String -> Map String Int -> String -> [[String]]
        numPaths' tk _ "end" = [["end"]]
        numPaths' tk smallVisited k = map (k:) nextPaths
            where
                nextPaths = concatMap (numPaths' tk smallVisited') next
                next = Set.toList ((mp Map.! k) Set.\\ Map.keysSet (Map.filter (>= 1) smallVisited))
                smallVisited' = if isBig k then smallVisited else Map.alter upd k smallVisited
                upd Nothing = Just 1
                upd (Just n) = Just (n+1)

task2v2 :: Map String (Set String) -> [[String]]
task2v2 mp =  (Set.toList . Set.fromList) $ map fixPath $ concatMap (task2' mp) smallKeys
  where fixPath = map fp
          where fp ('$':k) = k
                fp k = k
        smallKeys = filter (\k -> k /= "start" && k /= "end" && not (isBig k)) $ Map.keys mp
        task2' mp k = numPaths' (Set.singleton "start") "start"
            where kn = mp Map.! k
                  mp' = Map.insert ('$':k) kn mp
                  mp'' = foldr (Map.update (Just . Set.insert ('$':k))) mp' kn
                  numPaths' bigVisited "end" = [["end"]]
                  numPaths' smallVisited k = map (k:) nextPaths
                    where
                        nextPaths = concatMap (numPaths' smallVisited') next
                        next = Set.toList ((mp'' Map.! k) Set.\\ smallVisited)
                        smallVisited' = if isBig k
                                        then smallVisited
                                        else Set.insert k smallVisited

main :: IO ()
main = readInput "input" >>= print . length . task2