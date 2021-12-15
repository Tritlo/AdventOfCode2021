{-# LANGUAGE  ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Graph (Graph, Vertex)
import qualified Data.Graph as Graph
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Set as Set
import Data.Semigroup (Semigroup)
import qualified Data.IntMap as IMap
import qualified Data.IntSet as ISet
import Data.Char (intToDigit)
import Data.Function (on)

readInput :: FilePath -> IO [[Int]]
readInput = fmap (map (map (\d -> read [d])) . lines) . readFile

-- type AdjGraph =

instance Semigroup Int where
    (<>) = (+)
instance Monoid Int where
    mempty = 0
--task1 :: [[Int]] -> Int
--task1 :: [[Int]] -> [[(((Int, Int), Int), [((Int, Int), Int)])]]
--task1 :: [[Int]] -> Maybe Int
task1 :: [[Int]] -> (Maybe Int, [Char])
task1 nums@(fn:_) = (v , pathToStr (fmap (map (s3 . nodeFromVertex)) p) indices)
  where
      s3 (_,k,_) = k
      indices = map (map fst) $ indexed
      pathToStr _ [] =""
      pathToStr p ([]:rs) = '\n':pathToStr p rs
      pathToStr p ((c@(x,y):cs):rs) | Just path <- Set.fromList <$> p = (if c `Set.member` path
                                                                        then 'x' else dig):pathToStr p (cs:rs)
                              | otherwise = dig:(pathToStr p (cs:rs))
       where dig = (intToDigit ((nums !!x) !!y))

      (v, p) = dijkstra (Graph.graphFromEdges nmapmap) (fromJust $ vertexFromKey start) (fromJust $ vertexFromKey end)
      start = (0,0)
      end = (length fn -1, length nums -1)
      (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges nmapmap
      nmapmap = map (\((c,v), ns) -> (v,  c, map fst ns)) $ concat nmap
      indexed :: [[((Int, Int), Int)]]
      indexed = zipWith addInd [0..] $ map (zip [0..]) nums
        where addInd y = map (\(x,v) -> ((x,y), v))
      nmap = transpose $ map neighs2 $ transpose $ map neighs indexed
      neighs (n:n2:r) = (n,[n2]):neighs' (n:n2:r)
      neighs' [n2, n3] = [(n3, [n2])]
      neighs' (n:n2:n3:r) = (n2, [n,n3]):neighs' (n2:n3:r)

      neighs2 (n@(nv,nn):n2@(nv2,_):r) = (nv, nv2:nn):neighs2' (n:n2:r)
      neighs2' [(nv,_), (n2,nn)] = [(n2,nv:nn)]
      neighs2' ((nv,_):n2@(nv2,nn):n3@(nv3,_):r) = (nv2, nv:nv3:nn):neighs2' (n2:n3:r)

dijkstra :: (Monoid node, Ord node, Ord key) =>
 (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex) -> Vertex -> Vertex -> (Maybe node, Maybe [Vertex])
dijkstra (graph, nodeFromVertex, vertexFromKey) start goal =
                go initDist IMap.empty (ISet.fromList $ Graph.vertices graph)
    where initDist = IMap.singleton start mempty
          buildPath f t prev = bp' [] t
            where bp' cp cn | cn == f = Just (cn:cp)
                  bp' cp cn | Just nn <- prev IMap.!? cn = bp' (cn:cp) nn
                  bp' _ _ = Nothing
          go dist prev q | ISet.null q = (dist IMap.!? goal, buildPath start goal prev)
          go dist prev q | Just d <- dist' IMap.!? goal = (Just d, buildPath start goal prev')
                         | otherwise = go dist' prev' q'
            where u = minimumBy (dSort dist) $ ISet.toList q
                  q' = ISet.delete u q
                  (_, _, ns) = nodeFromVertex u
                  ninQ = filter (`ISet.member` q') $ mapMaybe vertexFromKey ns
                  (dist', prev') = foldr loop (dist, prev) ninQ
                  ud = dist IMap.! u
                  loop v (ldist, lprev) | Just vd <- ldist IMap.!? v,
                                          alt < vd = update
                                        | Just vd <- ldist IMap.!? v,
                                          alt >= vd = noupdate
                                        | otherwise = update
                    where (utov, _, _) = nodeFromVertex v
                          alt = ud <> utov
                          update = (IMap.insert v alt ldist, IMap.insert v u lprev)
                          noupdate = (ldist, lprev)

          dSort dist a b | Just ad <- dist IMap.!? a,
                           Just bd <- dist IMap.!? b = compare ad bd
                         | Just _ <- dist IMap.!? a = LT
                         | Just _ <- dist IMap.!? b = GT
                         | otherwise = EQ



--task2 :: [[Int]] -> (Maybe Int, [Char])
task2 :: [[Int]] -> (Maybe Int, [Char])
task2 inp = (v, pathToStr (fmap (map (s3 . nodeFromVertex)) p) indices)
  where
      nums@(fn:_) = transpose $ preproc $ transpose $ preproc inp
      preproc nums = [ concat [map (f a) r | a <- [0..4]] | r <- nums]
       where f a n = if (a+n) <= 9 then a+n else ((a+n) `mod` 9)


      s3 (_,k,_) = k
      indices = map (map fst) $ indexed
      pathToStr _ [] =""
      pathToStr p ([]:rs) = '\n':pathToStr p rs
      pathToStr p ((c@(x,y):cs):rs) | Just path <- Set.fromList <$> p = (if c `Set.member` path
                                                                        then 'x' else dig):pathToStr p (cs:rs)
                              | otherwise = dig:(pathToStr p (cs:rs))
       where dig = (intToDigit ((nums !!x) !!y))

      (v, p) = dijkstra (Graph.graphFromEdges nmapmap) (fromJust $ vertexFromKey start) (fromJust $ vertexFromKey end)
      start = (0,0)
      end = (length fn -1, length nums -1)
      (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges nmapmap
      nmapmap = map (\((c,v), ns) -> (v,  c, map fst ns)) $ concat nmap
      indexed :: [[((Int, Int), Int)]]
      indexed = zipWith addInd [0..] $ map (zip [0..]) nums
        where addInd y = map (\(x,v) -> ((x,y), v))
      nmap = transpose $ map neighs2 $ transpose $ map neighs indexed
      neighs (n:n2:r) = (n,[n2]):neighs' (n:n2:r)
      neighs' [n2, n3] = [(n3, [n2])]
      neighs' (n:n2:n3:r) = (n2, [n,n3]):neighs' (n2:n3:r)

      neighs2 (n@(nv,nn):n2@(nv2,_):r) = (nv, nv2:nn):neighs2' (n:n2:r)
      neighs2' [(nv,_), (n2,nn)] = [(n2,nv:nn)]
      neighs2' ((nv,_):n2@(nv2,nn):n3@(nv3,_):r) = (nv2, nv:nv3:nn):neighs2' (n2:n3:r)

-- Doesn't work!
-- We don't consider that going down then right is not the same as right
-- and then down.
task2Big nums = loop e2es [((0,0),0)]
    where
      preproc nums a = map (map f) nums
       where f n = if (a+n) <= 9 then a+n else ((a+n) `mod` 9)
      preprocced = map (preproc nums) [0..9]
      e2es = zip preprocced $ map task2v2 preprocced
      fromEdge edge = mapMaybe f
        where f (a,b,(v,_)) | a == edge = Just (b,v)
                            | b == edge = Just (a,v)
                            | otherwise = Nothing
      toOtherEdge nums (a,b) | a == 9 && b == 9 = [e1,e2]
                             | a == 9 = [e1]
                             | otherwise = [e2]
        where e1 = ((0,b), (nums !! 0) !! b)
              e2 = ((a,0), (nums !! a) !! b)
      frEdgeToNextEdge nums edgSoFar = concatMap f2 edgSoFar
        where f2 (e,v) = map ((v+) <$>) $ toOtherEdge nums e

      loop [(n9@(n91:_),g9)] curShortest = lookup end takePaths
         where ffe = flip fromEdge g9
               end = (length n91-1,length n9 -1)
               takePaths = map (minimumBy (compare `on` snd)) $
                            groupBy ((==) `on` fst)$
                            sortBy (compare `on` fst) $
                            concatMap (\(cs,csv) -> (map ((csv+) <$>) $ ffe cs)) curShortest

      loop ((n,g):r) curShortest = loop r toNex
        where ffe = flip fromEdge g
              takePaths = map (minimumBy (compare `on` snd)) $
                            groupBy ((==) `on` fst)$
                            sortBy (compare `on` fst) $
                            concatMap (\(cs,csv) -> (map ((csv+) <$>) $ ffe cs)) curShortest
              toNex = frEdgeToNextEdge n takePaths




task2v2 :: [[Int]] -> [((Int, Int), (Int, Int), (Int, [Char]))]
task2v2 nums@(fn:_) = map (\(a,b,(Just v, p)) -> (a,b,(v,p))) pairs
    --(v, pathToStr (fmap (map (s3 . nodeFromVertex)) p) indices)
  where
      preproc nums a = map (map f) nums
       where f n = if (a+n) <= 9 then a+n else ((a+n) `mod` 9)

      fullPToS p = pathToStr ((fmap (map (s3 . nodeFromVertex)) p)) indices
      edgeIndices = filter isEdge $ concat indices
        where isEdge (x,y) = x == 0 || x == fst end || y == 0 || y == snd end
      diffEdgePairs =
          nub $ sort $ map sortPair $ [(a,b) | a@(x1,y1) <- edgeIndices, b@(x2,y2)<- edgeIndices, x1 /= x2 && y1 /= y2]
        where sortPair (a,b) = if a < b then (a,b) else (b,a)
      pairs = [(a,b, fullPToS <$> runDijkstra nmapmap a b)| (a,b) <- diffEdgePairs]
      s3 (_,k,_) = k
      indices = map (map fst) $ indexed
      pathToStr _ [] =""
      pathToStr p ([]:rs) = '\n':pathToStr p rs
      pathToStr p ((c@(x,y):cs):rs) | Just path <- Set.fromList <$> p = (if c `Set.member` path
                                                                        then 'x' else dig):pathToStr p (cs:rs)
                              | otherwise = dig:(pathToStr p (cs:rs))
       where dig = (intToDigit ((nums !!x) !!y))

      runDijkstra mp start end = dijkstra (Graph.graphFromEdges mp) (fromJust $ vertexFromKey start) (fromJust $ vertexFromKey end)
      (v, p) = runDijkstra nmapmap start end
      start = (0,0)
      end = (length fn -1, length nums -1)
      (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges nmapmap
      nmapmap = map (\((c,v), ns) -> (v,  c, map fst ns)) $ concat nmap
      indexed :: [[((Int, Int), Int)]]
      indexed = zipWith addInd [0..] $ map (zip [0..]) nums
        where addInd y = map (\(x,v) -> ((x,y), v))
      nmap = transpose $ map neighs2 $ transpose $ map neighs indexed
      neighs (n:n2:r) = (n,[n2]):neighs' (n:n2:r)
      neighs' [n2, n3] = [(n3, [n2])]
      neighs' (n:n2:n3:r) = (n2, [n,n3]):neighs' (n2:n3:r)

      neighs2 (n@(nv,nn):n2@(nv2,_):r) = (nv, nv2:nn):neighs2' (n:n2:r)
      neighs2' [(nv,_), (n2,nn)] = [(n2,nv:nn)]
      neighs2' ((nv,_):n2@(nv2,nn):n3@(nv3,_):r) = (nv2, nv:nv3:nn):neighs2' (n2:n3:r)

main :: IO ()
main = do  v <- task2Big <$> readInput "input"
           print v