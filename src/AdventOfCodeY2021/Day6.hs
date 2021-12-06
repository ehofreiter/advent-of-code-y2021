module AdventOfCodeY2021.Day6 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

run = runWith "data/day6/input.txt"
test = runWith "data/day6/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let ints = readInts $ head strs
      fm = foldl' (flip ($)) Map.empty $ map insertFM ints
      fishes = (foldl' (.) id $ replicate 256 step2) fm
      c = sum $ Map.elems fishes
  print c

type Fish = Int

type FM = Map.Map Int Int

insertFM :: Int -> FM -> FM
insertFM x = Map.insertWith (+) x 1

step2 :: FM -> FM
step2 fs = fl''
  where
    fl = Map.fromList $ map (\(f, x) -> (f-1, x)) $ Map.toList fs
    fl' = Map.delete (-1) fl
    a = Map.lookup (-1) fl
    fl'' = case a of
      Nothing -> fl'
      Just n -> Map.insertWith (+) 6 n . Map.insertWith (+) 8 n $ fl'

-- step and stepAll used for part 1
step :: Int -> [Int]
step f = if f == 0 then [6, 8] else [f-1]

stepAll :: [Int] -> [Int]
stepAll fs = concatMap step fs

readInts :: String -> [Int]
readInts = map read . splitOn ","
