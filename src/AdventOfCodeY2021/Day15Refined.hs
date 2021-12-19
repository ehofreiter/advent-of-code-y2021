-- | Refined Day 15 solution, without time pressure.
-- Uses Dijkstra's algorithm instead of the ad-hoc search I initially came up
-- with. Expanding the cheapest paths first allows us to avoid having to re-check
-- neighbors after expanding, like in 'improvePaths' in 'Day15'.
module AdventOfCodeY2021.Day15Refined where

import Data.Foldable
import Data.Time
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import AdventOfCodeY2021.Common

import AdventOfCodeY2021.CoordVec as CV

run = runWith "data/day15/input.txt"
test = runWith "data/day15/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let rm = mkRM strs
      bigRm = mkBigRM rm
      p = search rm
      bigP = search bigRm
  t0 <- getCurrentTime
  print (fst p)
  t1 <- getCurrentTime
  print (fst bigP)
  t2 <- getCurrentTime
  print ("t1 - t0: " <> show (diffUTCTime t1 t0))
  print ("t2 - t1: " <> show (diffUTCTime t2 t1))

initState :: (Set.Set Coord, Set.Set Coord, PM)
initState = (initExpanded, initFrontier, Map.singleton (0,0) (0, [(0,0)]))

initFrontier :: Set.Set Coord
initFrontier = Set.singleton (0,0)

initExpanded :: Set.Set Coord
initExpanded = Set.empty

-- I wonder if it is worth making this more generic by replacing RM with Num a
-- => (Coord -> a).
search :: RM -> Path
search rm = loop initState
  where
    endCoord = (rowSize rm - 1, colSize rm - 1)
    loop :: (Set.Set Coord, Set.Set Coord, PM) -> Path
    loop s0 =
      let s1@(expanded, frontier, pm) = expandCheapestPath rm s0
      in  if Set.member endCoord expanded
          then pm Map.! endCoord
          else loop s1

-- | Find the cheapest path in the given frontier, and expand the frontier. Once
-- a node has been expanded, we know we have found the cheapest path to it (if
-- there were a cheaper path to it, then it would have been found earlier in the
-- search since we always expand the cheapest path first) and can exclude it
-- from the rest of the search.
--
-- This is the core iterative step of the algorithm. Uses a best-first search
-- known as Dijkstra's algorithm. Each iteration tracks the expanded nodes, the
-- frontier of the search, and the map of best paths found so far. The search
-- expands the cheapest (best) paths first, so once a node is expanded, we know
-- the optimal path to it from the starting node has been found.
expandCheapestPath
  :: RM
  -> (Set.Set Coord, Set.Set Coord, PM)
  -> (Set.Set Coord, Set.Set Coord, PM)
expandCheapestPath rm (expanded, frontier, pm) =
  let (coord, path) = minimumBy comparePairs
                    $ Map.toList $ pm `Map.restrictKeys` frontier
                    -- ^ NB: Initially I used 'pm `Map.withoutKeys` expanded',
                    -- which made this run approx 10x slower.
      adjs = filter (`Set.notMember` expanded) $ adjCoords4 rm coord
      coordPaths = map (\adj -> (adj, extendPath rm adj path)) adjs
      pm' = Map.unionWith betterPath pm (Map.fromList coordPaths)
      frontier' = foldl' (flip Set.insert) frontier (map fst coordPaths)
  in  (Set.insert coord expanded, Set.delete coord frontier', pm')
  where
    comparePairs (_, (cost1, _)) (_, (cost2, _)) = compare cost1 cost2
    betterPath path1@(cost1, _) path2@(cost2, _) =
      if cost1 < cost2
      then path1
      else path2

extendPath :: RM -> Coord -> Path -> Path
extendPath rm coord (cost, chain) = (cost + cost', coord:chain)
  where
    cost' = rm CV.! coord

comparePaths :: Path -> Path -> Ordering
comparePaths (cost1, _) (cost2, _) = compare cost1 cost2

type PM = Map.Map Coord Path -- best path to this coord

type Path = (Int, [Coord]) -- cost, chain

type RM = CoordVec Int

mkRM :: [String] -> RM
mkRM = fromLists . (map.map) (\c -> read [c])

mkBigRM :: RM -> RM
mkBigRM rm = fromLists risks
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
