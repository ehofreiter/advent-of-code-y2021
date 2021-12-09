module AdventOfCodeY2021.Day9 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.Maybe
import Data.List.Split
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

run = runWith "data/day9/input.txt"
test = runWith "data/day9/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let rows = zip strs [0..]
      hm = foldr insertRow Map.empty rows
      lps = filter (flip lp hm) $ Map.keys hm
      rs = map (\lp -> hm Map.! lp + 1) lps
      x = sum rs
      basins = map (\lp -> addBasin hm lp (Set.singleton lp)) lps
      sizes = map Set.size basins
      [a,b,c] = take 3 $ reverse $ sort sizes -- lol
  print x -- part 1
  print (a*b*c) -- part 2

type HM = Map.Map (Int, Int) Int

type Basin = Set.Set (Int, Int)

addBasin :: HM -> (Int, Int) -> Basin -> Basin
addBasin hm (x,y) basin =
  let nbcs = nborCoords (x,y) hm
      h = hm Map.! (x,y)
      cs = map fst $ filter ((/= 9) . snd) $ filter ((> h) . snd) nbcs
      basin' = Set.fromList cs `Set.union` basin
  in  foldl' (flip (addBasin hm)) basin' cs

insertRow :: (String, Int) -> HM -> HM
insertRow (s, y) hm = Map.union hm $ Map.fromList $ zipWith f s [0..]
  where
    f c x = ((x,y), read [c])

lp :: (Int, Int) -> HM -> Bool
lp (x,y) hm = all (> h) (neighbors (x,y) hm)
  where
    h = hm Map.! (x,y)

neighbors :: (Int, Int) -> HM -> [Int]
neighbors (x, y) hm =
  mapMaybe (flip Map.lookup hm) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

nborCoords :: (Int, Int) -> HM -> [((Int,Int),Int)]
nborCoords (x, y) hm =
  mapMaybe f [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
  where
    f c = case Map.lookup c hm of
      Nothing -> Nothing
      Just h -> Just (c, h)
