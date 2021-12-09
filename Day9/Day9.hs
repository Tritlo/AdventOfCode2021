module Main where
import Data.List (transpose, sort)
import qualified Data.Set as Set


readInput :: FilePath -> IO [[Int]]
readInput = fmap (map line . lines) . readFile
  where line :: String -> [Int]
        line = map cToD
          where cToD c = read [c]

task1 :: [[Int]] -> Int
task1 nums = sum $ map ( sum . map ((1+) . fst) . filter snd . map isLow) nmap
  where
      isLow (n, r) = (n, all (n <) r)
      nmap = transpose $ map neighs2 $ transpose $ map neighs nums
      neighs (n:n2:r) = (n,[n2]):neighs' (n:n2:r)
      neighs' [n2, n3] = [(n3, [n2])]
      neighs' (n:n2:n3:r) = (n2, [n,n3]):neighs' (n2:n3:r)

      neighs2 (n@(nv,nn):n2@(nv2,_):r) = (nv, nv2:nn):neighs2' (n:n2:r)
      neighs2' [(nv,_), (n2,nn)] = [(n2,nv:nn)]
      neighs2' ((nv,_):n2@(nv2,nn):n3@(nv3,_):r) = (nv2, nv:nv3:nn):neighs2' (n2:n3:r)

task2 :: [[Int]] -> Int
task2 nums = product $ take 3 $ reverse $ sort basins
  where
      lps = concatMap (filter snd) wbCheck
      isLow (n@(_,v), r) = ((n, filter ((/= 9) . snd) r)
                             , all ((v <) . snd) r)
      postLp = map (map fst) wbCheck
      wbCheck = map (map isLow) nmap
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
      basins = map (Set.size . findFromLp) lps
      findFromLp (((c,v),ns), _) = untilNoNew (Set.singleton c) $ map fst ns
        where untilNoNew found [] = found
              untilNoNew found (coord:rest)
               | coord `Set.member` found = untilNoNew found rest
               | otherwise = untilNoNew (coord `Set.insert` found) (rest ++ getNs coord)
              getNs (x,y) = map fst $ snd $ (postLp !! y) !! x

main :: IO ()
main = readInput "input" >>= print . task2