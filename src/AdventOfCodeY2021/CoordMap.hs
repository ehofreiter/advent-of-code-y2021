module AdventOfCodeY2021.CoordMap where

import           Data.List
import qualified Data.Map.Strict as Map

-- Map from 2d integer coordinates to values.

type Coord = (Int, Int)
type CoordMap a = Map.Map Coord a

-- | Makes a coordinate map from a list of rows where coordinates are
-- (row, column). If you consider the coordinates as x and y with x horizontal
-- and y vertical, the coordinates are like (y, x).
mkCoordMap :: [[a]] -> CoordMap a
mkCoordMap = Map.fromList . concat . zipWith mkRow [0..]
  where
    mkRow x = zipWith (mkKeyValue x) [0..]
    mkKeyValue x y a = ((x, y), a)

-- | Essentially the inverse of 'mkCoordMap'.
unCoordMap :: CoordMap a -> [[a]]
unCoordMap cm = getRows $ Map.assocs cm
  where
    getRows [] = []
    getRows pairs@(((x, y), a) : ps) =
      let (r, pairs') = span ((== x) . fst . fst) pairs
      in  map snd r : getRows pairs'

bound :: CoordMap a -> [Coord] -> [Coord]
bound cm cs = cs `intersect` Map.keys cm

adjCoordsHoriz_ :: CoordMap a -> Coord -> [Coord]
adjCoordsHoriz_ cm (x, y) = [ (x + 1, y), (x - 1, y) ]

adjCoordsVert_ :: CoordMap a -> Coord -> [Coord]
adjCoordsVert_ cm (x, y) = [ (x, y + 1), (x, y - 1) ]

-- | The 4 adjacent coordinates horizontally and vertically, including out of
-- bounds coordinates.
adjCoords4_ :: CoordMap a -> Coord -> [Coord]
adjCoords4_ cm c = adjCoordsHoriz_ cm c ++ adjCoordsVert_ cm c

-- | The 4 adjacent coordinates horizontally and vertically.
adjCoords4 :: CoordMap a -> Coord -> [Coord]
adjCoords4 cm c = bound cm (adjCoords4_ cm c)

adjCoordsDiag_ :: CoordMap a -> Coord -> [Coord]
adjCoordsDiag_ cm (x, y) =
  [ (x + 1, y + 1), (x + 1, y - 1)
  , (x - 1, y + 1), (x - 1, y - 1)
  ]

-- | The 8 adjacent coordinates horizontally, vertically, and diagonally,
-- including out of bounds coordinates.
adjCoords8_ :: CoordMap a -> Coord -> [Coord]
adjCoords8_ cm c = adjCoords4_ cm c ++ adjCoordsDiag_ cm c

-- | The 8 adjacent coordinates horizontally, vertically, and diagonally.
adjCoords8 :: CoordMap a -> Coord -> [Coord]
adjCoords8 cm c = bound cm (adjCoords8_ cm c)

adjs4 :: CoordMap a -> Coord -> [a]
adjs4 cm c = (cm Map.!) <$> adjCoords4_ cm c

adjs8 :: CoordMap a -> Coord -> [a]
adjs8 cm c = (cm Map.!) <$> adjCoords8 cm c
