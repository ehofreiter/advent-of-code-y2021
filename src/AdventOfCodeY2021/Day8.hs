module AdventOfCodeY2021.Day8 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

run = runWith "data/day8/input.txt"
test = runWith "data/day8/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let entries = map readEntry strs
      outputs = concatMap snd entries
      c = length . filter (\l -> l == 2 || l == 4 || l == 7 || l == 3) $ map length $ outputs
      numMaps = map (\(x, y) -> (translate x, y)) entries
      ints = map (\(nm, y) -> makeInt nm y) numMaps
  print c -- Part 1
  print (sum ints) -- Part 2

makeInt :: NumMap -> [String] -> Int
makeInt nm ss = sum $ zipWith g ds [0..]
  where
    f s = nm Map.! Set.fromList s
    ds = reverse $ map f ss
    g d i = d * 10^i

type NumMap = Map.Map (Set.Set Char) Int

translate :: [String] -> NumMap
translate s =
  let Just one = find ((== 2) . length) s
      Just four = find ((== 4) . length) s
      Just seven = find ((== 3) . length) s
      Just eight = find ((== 7) . length) s
      twoThreeFive = filter ((== 5) . length) s
      zeroSixNine = filter ((== 6) . length) s
      Just three = find ((== 2) . length . intersect one) twoThreeFive
      Just two = find ((== 2) . length . intersect four) twoThreeFive
      [five] = twoThreeFive \\ [two, three]
      Just six = find ((== 1) . length . intersect one) zeroSixNine
      Just nine = find ((== 4) . length . intersect four) zeroSixNine
      [zero] = zeroSixNine \\ [six, nine]
  in  Map.fromList . map (\(x, y) -> (Set.fromList y, x)) $ [(1, one), (2, two), (3, three), (4, four), (5, five), (6, six), (7, seven), (8, eight), (9, nine), (0, zero)]

readEntry :: String -> ([String], [String])
readEntry s = (take 10 $ words s, drop 11 $ words s)
