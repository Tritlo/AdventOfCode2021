module Main where
import Data.List (transpose)


readInput :: FilePath -> IO [[Int]]
readInput = fmap (map line . lines) . readFile
  where line :: String -> [Int]
        line = map cToD
          where cToD c = read [c]

task1 :: [[Int]] -> Int
task1 nums = sum . map (sum . map ((1+) . fst)) $ map (filter snd) $ map (map isLow) nmap
  where
      isLow (n, r) = (n, all (n <) r)
      nmap = transpose $ map neighs2 $ transpose $ map neighs nums
      neighs (n:n2:r) = (n,[n2]):neighs' (n:n2:r)
      neighs' [n2, n3] = [(n3, [n2])]
      neighs' (n:n2:n3:r) = (n2, [n,n3]):neighs' (n2:n3:r)

      neighs2 (n@(nv,nn):n2@(nv2,_):r) = (nv, nv2:nn):neighs2' (n:n2:r)
      neighs2' [(nv,_), (n2,nn)] = [(n2,nv:nn)]
      neighs2' ((nv,_):n2@(nv2,nn):n3@(nv3,_):r) = (nv2, nv:nv3:nn):neighs2' (n2:n3:r)

main :: IO ()
main = readInput "input" >>= print . task1