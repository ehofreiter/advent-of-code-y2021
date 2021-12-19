module AdventOfCodeY2021.Day15 where

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

import AdventOfCodeY2021.CoordVec as CV

run = runWith "data/day15/input.txt"
test = runWith "data/day15/test.txt"

--runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let rm = bigRM $ mkRM strs
      (maxX, maxY) = (rowSize rm-1, colSize rm-1)
      initState = (1, Map.singleton (0,0) (0, [(0,0)]))
      (l, pm) = iterate' (stepBests rm) initState !! (maxX+maxY)
  --mapM_ print (toLists rm)
  --print $ chunksOf 100 $ last $ toLists rm
  print (fst $ pm Map.! (maxX,maxY)) -- takes approx 3 minutes to run
  --pure (rm, l, pm)

type PM = Map.Map Coord Path -- best path to this coord

stepBests :: RM -> (Int, PM) -> (Int, PM)
stepBests rm (level, pm) =
  let coords = getLevelCoords level rm
  in  if level > maxLevel
      then (level, pm)
      else (level + 1, foldl' (addBestPathTo2 rm) pm coords)
  where
    maxLevel = rowSize rm - 1 + colSize rm - 1

getLevelCoords :: Int -> RM -> [Coord]
getLevelCoords level rm =
  let allCoords = [ (x, y) | x <- [0..rowSize rm-1], y <- [0..colSize rm-1] ]
  in  filter (\(x,y) -> x+y == level) allCoords

addBestPathTo :: RM -> PM -> Coord -> PM
addBestPathTo rm pm c =
  let adjs = filter (\(x,y) -> x <= fst c && y <= snd c) $ adjCoords4 rm c
      (bestCost, bestPath) = minimumBy m $ map (pm Map.!) adjs
  in  Map.insert c (rm CV.! c + bestCost, c:bestPath) pm
  where
    m x y = compare (fst x) (fst y)

addBestPathTo2 :: RM -> PM -> Coord -> PM
addBestPathTo2 rm pm c =
  let adjs = filter (\(x,y) -> x <= fst c && y <= snd c) $ adjCoords4 rm c
      paths = map (pm Map.!) adjs
      (bestCost, bestPath) = minimumBy m paths
      (bestCost', bestPath') = (rm CV.! c + bestCost, c:bestPath)
  in  improvePaths rm (Map.insert c (bestCost', bestPath') pm) c
  where
    m x y = compare (fst x) (fst y)

-- interestingly, this type of backtracking wasn't needed until running
-- part 2 on the puzzle input. even the test input for part 2 didn't need
-- backtracking!
improvePaths :: RM -> PM -> Coord -> PM
improvePaths rm pm c =
  let adjs = adjCoords4 rm c
      (thisCost, thisPath) = pm Map.! c
      paths = mapMaybe (`Map.lookup` pm) adjs
      badPaths = mapMaybe f paths
      f (cost, path) =
        let newCost = thisCost + (rm CV.! head path)
        in  if newCost < cost
            then Just (newCost, head path:thisPath)
            else Nothing
      pm' = foldl' insertPath pm badPaths
      insertPath pmx (cx, px) = improvePaths rm (Map.insert (head px) (cx, px) pmx) (head px)
  in pm'

type Path = (Int, [Coord]) -- cost, path

type RM = CoordVec Int

mkRM :: [String] -> RM
mkRM = fromLists . (map.map) (\c -> read [c])

bigRM :: RM -> RM
bigRM rm = fromLists risks
  where
    risks = (map.map) f coords
    f (x,y) =
      let r = rm CV.! (x `mod` tileX, y `mod` tileX) + x `div` tileX + y `div` tileY
      in  if r >= 10 then r - 9 else r
    coords = [[(x,y) | x <- [0..bigX-1]] | y <- [0..bigY-1]]
    tileX = rowSize rm
    tileY = colSize rm
    bigX = rowSize rm * 5
    bigY = colSize rm * 5
