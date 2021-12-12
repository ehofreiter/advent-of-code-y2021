module AdventOfCodeY2021.Common where

import qualified Data.Map.Strict as Map
import           Data.List.Split

readInputs :: FilePath -> (String -> a) -> IO [a]
readInputs filePath f = do
  fileContents <- readFile filePath
  pure (f <$> lines fileContents)

readIntList :: String -> [Int]
readIntList = map read . splitOn ","

{-
-- Started drafting some common code for a 2d coordinate map.
-- A couple puzzles used it so far, so might be helpful to have.

type Coord = (Int, Int)
type CoordMap a = Map.Map Coord a

mkCoordMap :: [[a]] -> CoordMap a
mkCoordMap = Map.fromList . concat . zipWith mkRow [0..]
  where
    mkRow y = zipWith (mkKeyValue y) [0..]
    mkKeyValue y x a = ((x, y), a)

adjCoordsHoriz :: CoordMap a -> Coord -> [Coord]
adjCoordsHoriz cm (x, y) = [ (x + 1, y), (x - 1, y) ]

adjCoordsVert :: CoordMap a -> Coord -> [Coord]
adjCoordsVert cm (x, y) = [ (x, y + 1), (x, y - 1) ]

adjCoordsEdge :: CoordMap a -> Coord -> [Coord]
adjCoordsEdge cm c = adjCoordsHoriz ++ adjCoordsVert

adjCoordsDiag :: CoordMap a -> Coord -> [Coord]
adjCoordsDiag cm (x, y) =
  [ (x + 1, y + 1), (x + 1, y - 1)
  , (x - 1, y + 1), (x - 1, y - 1)
  ]

adjCoordsCorner :: CoordMap a -> Coord -> [Coord]
adjCoordsCorner cm (x, y) =
  adjCoords cm (x, y) ++ adjCoordsDiag
-}
