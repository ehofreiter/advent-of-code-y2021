module AdventOfCodeY2021.Day20 where

import Control.Applicative
import Control.Lens
import Control.Monad
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

import AdventOfCodeY2021.CoordMap as CM

runF = "data/day20/input.txt"
testF = "data/day20/test.txt"

run = runWith "data/day20/input.txt"
test = runWith "data/day20/test.txt"

load :: FilePath -> IO Input
load filePath = do
  strs <- readInputs filePath id
  let enh = Map.fromList $ zip [0..] (map readSpot $ head strs)
      image0 = mkCoordMap $ (map.map) readSpot $ drop 1 $ tail strs
  pure (enh, image0)

runWith :: FilePath -> IO ()
runWith filePath = do
  (enh, cm) <- load filePath
  let cm' = stepCM enh (False, cm)
      cm'' = stepCM enh cm'
      part1 = length $ filter id $ Map.elems (snd cm'')
      cm2 = iterate' (stepCM enh) (False, cm) !! 50
      part2 = length $ filter id $ Map.elems (snd cm2)
  print part2

stepCM :: Enh -> (Bool, CoordMap Bool) -> (Bool, CoordMap Bool)
stepCM enh (def, cm) =
  let cm' = expand (def, cm)
      cm'' = Map.mapWithKey (\k _ -> updateAt enh (def, cm) k) cm'
  in  (not def, cm'')

expand :: (Bool, CoordMap Bool) -> CoordMap Bool
expand (def, cm) =
  let (minX, minY) = minimum $ Map.keys cm
      (maxX, maxY) = maximum $ Map.keys cm
      xs = [minX-1..maxX+1]
      ys = [minY-1..maxY+1]
      xys =  [(x,y) | x <- [minX-1,maxX+1], y <- ys]
          ++ [(x,y) | y <- [minY-1,maxY+1], x <- xs]
  in  foldl' (\m k -> Map.insert k def m) cm xys

printCM :: CoordMap Bool -> IO ()
printCM cm = mapM_ print $ (map.map) showSpot $ unCoordMap cm

showSpot :: Bool -> Char
showSpot True = '#'
showSpot False = '.'

updateAt :: Enh -> (Bool, CoordMap Bool) -> (Int, Int) -> Bool
updateAt enh (def,cm) (x,y) =
  let adjs = adj (x,y)
      index = bitsToInt $ map (fromMaybe def . flip Map.lookup cm) adjs
  in  enh Map.! index

bitsToInt :: [Bool] -> Int
bitsToInt bs = sum $ zipWith f [0..] (reverse bs)
  where
    f i True = 2^i
    f i False = 0

adj :: (Int, Int) -> [(Int, Int)]
adj v = map (addC v) $ (,) <$> [-1,0,1] <*> [-1,0,1]

addC :: (Int, Int) -> (Int, Int) -> (Int, Int)
addC (x0, y0) (x1, y1) = (x0+x1, y0+y1)

type Enh = Map.Map Int Bool
type Input = (Map.Map Int Bool, CoordMap Bool)

readSpot :: Char -> Bool
readSpot '#' = True
readSpot '.' = False
