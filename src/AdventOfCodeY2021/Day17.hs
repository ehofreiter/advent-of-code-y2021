module AdventOfCodeY2021.Day17 where

import Control.Lens
import Data.Char
import Data.Foldable
import Data.List
import Linear
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq((:|>), (:<|)), (><))
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

import AdventOfCodeY2021.CoordVec as CV

test, run :: TargetArea
test = (V2 20 (-10), V2 30 (-5))
run = (V2 230 (-107), V2 283 (-57))

type Pos = V2 Int
type Vel = V2 Int

type TargetArea = (Pos, Pos) -- minX minY maxX maxY

-- Part 2
getGood :: TargetArea -> [(Int, Int)]
getGood ta = [ (x, y) | y <- getGoodYs ta, x <- getGoodXs ta y ]

getGoodYs :: TargetArea -> [Int]
getGoodYs ta =
  mapMaybe (\y -> Just y <* trajectoryY ta (V2 0 y)) [-200..1000]

getGoodXs :: TargetArea -> Int -> [Int]
getGoodXs ta y =
  mapMaybe (\x -> trajectoryAllX ta (V2 x y)) [0..300]

-- Part 1
searchY :: TargetArea -> Maybe Int
searchY ta =
  maximum $ map (\y -> trajectoryY ta (V2 0 y)) [0..1000]

trajectoryY :: TargetArea -> Vel -> Maybe Int
trajectoryY ta v0 =
  let ts = evalByY ta $ iterate' forward (v0, zero)
      (o, (v, p)) = last ts
  in  if o == EQ
      then Just (maxY ts)
      else Nothing

trajectoryAllX :: TargetArea -> Vel -> Maybe Int
trajectoryAllX ta v0 =
  let ts = evalByAll ta $ iterate' forward (v0, zero)
      (o, (v, V2 px py)) = last ts
  in  if o == EQ
      then Just px
      else Nothing

trajectory :: TargetArea -> Vel -> [(Ordering, (Vel, Pos))]
trajectory ta v0 = evalByY ta $ iterate' forward (v0, zero)

maxY :: [(Ordering, (Vel, Pos))] -> Int
maxY = maximum . map f
  where
    f (_, (_, V2 _ y)) = y

forward :: (Vel, Pos) -> (Vel, Pos)
forward (v@(V2 vx vy), p) = (v', p')
  where
    p' = p ^+^ v
    v' = V2 (update vx) (vy - 1)
    update vx | vx > 0    = vx - 1
              | vx < 0    = vx + 1
              | otherwise = vx

evalByY :: TargetArea -> [(Vel, Pos)] -> [(Ordering, (Vel, Pos))]
evalByY ta vps = takeWhile ((/= LT) . fst) $ zip (map (evalY ta) vps) vps

evalByAll :: TargetArea -> [(Vel, Pos)] -> [(Ordering, (Vel, Pos))]
evalByAll ta vps = takeWhile ((/= LT) . fst) $ zip (map (evalAll ta) vps) vps

evalY :: TargetArea -> (Vel, Pos) -> Ordering
evalY (V2 minX minY, V2 maxX maxY) (v, V2 px py)
  | py < minY = LT
  | py > maxY = GT
  | otherwise = EQ -- in target area

evalAll :: TargetArea -> (Vel, Pos) -> Ordering
evalAll (V2 minX minY, V2 maxX maxY) (v, V2 px py)
  | py < minY = LT
  | py > maxY = GT
  | px < minX = GT -- GT means not yet there
  | px > maxX = LT -- LT means overshot
  | otherwise = EQ -- in target area
