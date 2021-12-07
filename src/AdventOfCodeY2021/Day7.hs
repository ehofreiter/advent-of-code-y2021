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
--totalCost target sources = sum $ map (cost target) sources
totalCost target sources = sum $ map (cost2 target) sources
--totalCost target sources = sum $ map (cost2slow target) sources

-- Part 1 cost function
cost :: Int -> Int -> Int
cost target source = abs (target - source)

-- Part 2 cost function
cost2 :: Int -> Int -> Int
cost2 target source = (n * (n + 1)) `div` 2
  where
    n = abs (target - source)

-- Was Part 2 an efficiency trap?
-- Maybe the more straightforward approach of actually performing the sum 1 + 2
-- + ... + n makes it too slow? Let's try it with the below cost function.
-- Result: It's pretty slow, but still solved it in less than a minute.
cost2slow :: Int -> Int -> Int
cost2slow target source = sum [1..n]
  where
    n = abs (target - source)
