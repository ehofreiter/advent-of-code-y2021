module AdventOfCodeY2021.Day5 where

import Data.List
import Data.List.Split
import Data.Foldable
import Data.Traversable
import qualified Data.Map.Strict as Map

import AdventOfCodeY2021.Common

run = runWith "data/day5/input.txt"
test = runWith "data/day5/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let vents = map readVent strs
      hlines = map mkHLine vents
      vlines = map mkVLine vents
      dlines = map mkDLine vents
      -- Below line was for part 1
      -- board = foldl' (flip insertL) Map.empty (hlines ++ vlines)
      board = foldl' (flip insertL) Map.empty (hlines ++ vlines ++ dlines)
      c = length . filter ((> 1) . snd) $ Map.toList board
  --print board
  --print vents
  --print strs
  print c

type Coord = (Int, Int)
type Vent = (Coord, Coord)
type Line = [Coord]
type Board = Map.Map Coord Int

readVent :: String -> Vent
readVent s = (readCoord a, readCoord b)
  where
    a = words s !! 0
    b = words s !! 2

readCoord :: String -> Coord
readCoord s = (x, y)
  where
    x = read $ splitOn "," s !! 0
    y = read $ splitOn "," s !! 1

mkDLine :: Vent -> Line
mkDLine ((x1, y1), (x2, y2)) | x1 /= x2 && y1 /= y2 = zip xs ys
                             | otherwise = []
  where
    xs = if x1 < x2 then [x1..x2] else reverse [x2..x1]
    ys = if y1 < y2 then [y1..y2] else reverse [y2..y1]

mkHLine :: Vent -> Line
mkHLine ((x1, y1), (x2, y2)) | y1 == y2 = map (\x -> (x, y1)) xs
                             | otherwise = []
  where
    xs = [min x1 x2 .. max x1 x2]

mkVLine :: Vent -> Line
mkVLine ((x1, y1), (x2, y2)) | x1 == x2 = map (\y -> (x1, y)) ys
                             | otherwise = []
  where
    ys = [min y1 y2 .. max y1 y2]

insertC :: Coord -> Board -> Board
insertC c = Map.insertWith (+) c 1

insertL :: Line -> Board -> Board
insertL l b = foldl' (flip insertC) b l
