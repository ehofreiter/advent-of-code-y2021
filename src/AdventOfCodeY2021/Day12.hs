module AdventOfCodeY2021.Day12 where

import Control.Lens
import Data.Char
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

run = runWith "data/day12/input.txt"
test = runWith "data/day12/test.txt"
test2 = runWith "data/day12/test2.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let edges = map readEdge strs
      cm = mkCM edges
      paths = stepPath cm []
      c = length paths
      paths2 = stepPath2 cm (Nothing,[])
      c2 = length paths2
  --print (take 2 paths)
  print c -- part 1
  --print (take 2  $ filter ((/= "end") . snd . head . snd) paths2)
  --mapM (print . map snd . reverse . snd) paths2
  print c2 -- part 2

readEdge :: String -> (Cave, Cave)
readEdge s = (readCave a, readCave b)
  where
    [a,b] = splitOn "-" s

readCave :: String -> Cave
readCave s = (isBig, s)
  where
    isBig = isUpper (head s)

mkCM :: [(Cave, Cave)] -> CM
mkCM = foldl' addEdge Map.empty

addEdge :: CM -> (Cave, Cave) -> CM
addEdge cm (a, b) =
  let cm' = Map.insertWith (Set.union) a (Set.singleton b) cm
  in  Map.insertWith (Set.union) b (Set.singleton a) cm'

type CM = Map.Map Cave (Set.Set Cave)

type Cave = (Bool, String) -- isBig, name

type Path = [Cave]
type Path2 = (Maybe Cave, [Cave])

stepPath :: CM -> Path -> [Path]
stepPath cm [] = stepPath cm [(False, "start")]
stepPath cm (c:cs) =
  if snd c == "end"
  then [c:cs]
  else concatMap (stepPath cm . (: (c:cs))) $ Set.toList nexts
  where
    nexts = Set.filter f $ cm Map.! c
    f (True, _) = True
    f (False, s) = s `notElem` map snd cs

stepPath2 :: CM -> Path2 -> [Path2]
stepPath2 cm (_,[]) = stepPath2 cm (Nothing, [(False, "start")])
stepPath2 cm (d@(Just _), (c:cs)) =
  if snd c == "end"
  then [(d, c:cs)]
  else concatMap (\n -> stepPath2 cm (d,(n : (c:cs)))) $ Set.toList nexts
  where
    nexts = Set.filter f $ cm Map.! c
    f (True, _) = True
    f (False, s) = s `notElem` map snd cs
stepPath2 cm (Nothing, (c:cs)) =
  if snd c == "end"
  then [(Nothing, c:cs)]
  else concatMap (\(d,n) -> stepPath2 cm (d,(n : (c:cs)))) $ nexts
  where
    nexts = mapMaybe f $ Set.toList $ cm Map.! c
    f c@(True, f) = Just (Nothing, c)
    f c@(False, "start") = Nothing
    f c@(False, s) = if s `elem` map snd cs
                     then Just (Just c, c)
                     else Just (Nothing, c)
