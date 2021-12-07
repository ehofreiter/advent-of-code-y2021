module AdventOfCodeY2021.Day7 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

run = runWith "data/day7/input.txt"
test = runWith "data/day7/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let ints = readInts $ head strs
      targets = [minimum ints..maximum ints]
      results = map (\t -> (t, totalCost t ints)) targets
      r = minimumBy (\x y -> compare (snd x) (snd y)) results
  print r

readInts :: String -> [Int]
readInts = map read . splitOn ","

totalCost :: Int -> [Int] -> Int
totalCost target sources = sum $ map (cost target) sources

-- This is part 2, part 1 was just abs (target - source)
cost :: Int -> Int -> Int
cost target source = (n * (n + 1)) `div` 2
  where
    n = abs (target - source)
