module AdventOfCodeY2021.Day19 where

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

import AdventOfCodeY2021.CoordVec as CV

run = runWith "data/day19/input.txt"
test = runWith "data/day19/test.txt"

--runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let scannerMap = Map.fromList $ map readScanner $ splitOn [""] strs
      -- s0 = scannerMap Map.! 0
      -- s1 = scannerMap Map.! 1
      --Just (o, t, ol) = matchScanners s0 s1
      --coordSet = stepAll scannerMap
      --part1 = Set.size coordSet
      (transforms, sm') = solveScannerMap scannerMap
      scannerPositions = map snd $ Map.elems $ transforms
      part1 = Set.size $ Set.fromList $ concatMap Map.elems (Map.elems sm')
      part2 = maximum $ map (uncurry manhattanDist) $ choose2 scannerPositions
  --pure scannerMap
  --print part1
  print part2

manhattanDist :: V3 Int -> V3 Int -> Int
manhattanDist u v = sum $ abs <$> u - v

-- | Takes the initial puzzle input and yields a scanner map with all scanner
-- beacons transformed to be relative to scanner 0, as well as a map giving the
-- transform that was applied to each scanner to get its coordinates from
-- initial to solved. Starts by assuming scanner 0 is solved relative to itself,
-- then each iterative step finds and solves one additional scanner.
solveScannerMap :: ScannerMap -> (TransformMap, ScannerMap)
solveScannerMap =
  loop initKnown
  where
    initKnown = Map.singleton 0 (identity, zero)
    loop known sm =
      if Map.size known == Map.size sm
      then (known, sm)
      else let (known', sm') = stepScannerMap (known, sm)
           in  loop known' sm'

-- | Finds the next unsolved scanner and solves its position and orientation
-- relative to scanner 0. Adds the unsolved scanner to the list of known scanners
-- along with its position and orientation, and updates its beacon coordinates
-- in the scanner map to be relative to scanner 0.
stepScannerMap :: (TransformMap, ScannerMap) -> (TransformMap, ScannerMap)
stepScannerMap (knownScanners, sm) =
  let knownKeys = Map.keys knownScanners
      unknownKeys = Map.keys sm \\ knownKeys
      searchPairs = (,) <$> knownKeys <*> unknownKeys
      -- ^ most time consuming bug: I had this as 'zip knownKeys unknownKeys',
      -- which tries only a frustratingly unpredictable subset of scanner pairs
      -- and makes you think you're crazy
      matchKeys k0 k1 = case matchScanners (sm Map.! k0) (sm Map.! k1) of
        Nothing -> Nothing
        Just result -> Just (k1, result)
      (k1, (r, t, ol)) = head $ mapMaybe (uncurry matchKeys) searchPairs
      known' = Map.insert k1 (r, t) knownScanners
      sm' = Map.adjust (Map.map (\c -> r !* c ^+^ t)) k1 sm
  in (known', sm')

-- Beacon ID -> beacon position coordinate
type CoordMap = Map.Map Int (V3 Int)
-- (Beacon ID A, Beacon ID B) -> position A - position B
type DiffMap = Map.Map (Int, Int) (V3 Int)
-- Scanner ID -> beacon coordinates
type ScannerMap = Map.Map Int CoordMap
-- Scanner ID -> (rotation matrix, translation vector)
type TransformMap = Map.Map Int (M33 Int, V3 Int)

-- | Check if two scanner coordinate maps have 12 matching beacons. Yields
-- 'Nothing' if no match, else yields just the rotation matrix and translation
-- vector that can be applied to the second coordinate map to make it in terms
-- of the first. Also yields the set of coordinate diffs that matched, mainly
-- for debugging.
--
-- First, for each scanner, take all its n*(n-1) beacon coordinate differences.
-- Then compare the difference magnitudes between the two scanners to see if we
-- get the expected number of matches (12*11 = 132 matching diffs for 12
-- matching beacons). If so, find the one of 24 possible orientations that makes
-- the diffs match.
matchScanners :: CoordMap -> CoordMap -> Maybe (M33 Int, V3 Int, Overlap)
matchScanners cs0 cs1 =
  let cs0Diffs = diffs cs0
      cs1Diffs = diffs cs1
      cs1Diffs' = refineDiffsBy cs0Diffs cs1Diffs
      cs1OrientDiffs = map (\o -> (o, Map.map (o !*) cs1Diffs')) orients
      overlaps = map (diffOverlap cs0Diffs <$>) cs1OrientDiffs
  in do
    -- NB: 66 = 12 choose 2. The guard/filter on >= 66 is a heuristic for when
    -- we think we have 12 matching beacons. Because the diffs actually include
    -- all *ordered* pairs (e.g. (a,b) as well as (b,a)), there will actually be
    -- 132 matching diffs when 12 beacons match. But this still worked. Would be
    -- interesting to see how changing this affects the efficiency. A higher
    -- number rules out more false positives, but if there are no false positives
    -- above a certain threshold (seems to be the case for 66), you can do a bit
    -- less work for the true positives.
    guard (Map.size cs1Diffs' >= 66)
    (r, ol) <- find ((>= 66) . length . snd) $ overlaps
    let ((c0,_), (c1,_), _) = head ol
        t = cs0 Map.! c0 ^-^ r !* cs1 Map.! c1
    pure (r, t, ol)

refineDiffsBy :: DiffMap -> DiffMap -> DiffMap
refineDiffsBy dm0 dm1 =
  let dd0 = Set.fromList $ map quadrance $ Map.elems dm0
  in  Map.filter ((`Set.member` dd0) . quadrance) dm1

type Overlap = [((Int, Int), (Int, Int), V3 Int)]

matchingCoords :: Overlap -> Map.Map (Int, Int) Int
matchingCoords = mkHistogram . concatMap f
  where
    f ((a0,b0), (c0,d0), _) = [(a0,c0), (b0,d0)]

diffOverlap :: DiffMap -> DiffMap -> Overlap
diffOverlap dm0 dm1 =
  catMaybes $ f <$> Map.toList dm0 <*> Map.toList dm1
  where
    f (ci0, v0) (ci1, v1) = if v0 == v1 then Just (ci0, ci1, v0) else Nothing

diffs :: Map.Map Int (V3 Int) -> Map.Map (Int, Int) (V3 Int)
diffs vs = Map.fromList $ catMaybes $ f <$> Map.toList vs <*> Map.toList vs
  where
    f (k0, v0) (k1, v1) | k0 == k1 = Nothing
                        | otherwise = Just ((k0, k1), v0 ^-^ v1)

-- | Caches the 24 different non-reflecting 3d orthogonal rotations. Starts with
-- all orthogonal rotations and filters out the ones that have determinant of
-- -1, which are the ones that include a reflection.
orients :: [M33 Int]
orients = filter ((== 1) . det33) allOrients
  where
    allOrients = do
      let cols = [V3 1 0 0, V3 0 1 0, V3 0 0 1]
      [vx, vy, vz] <- permutations cols
      applyFlips (V3 vx vy vz)
    applyFlips v3 = map (v3 ^*) flips
    flips = V3 <$> [-1,1] <*> [-1,1] <*> [-1,1]

type Scanner = (Int, Map.Map Int (V3 Int))

readScanner :: [String] -> Scanner
readScanner s = (sNum, Map.fromList $ zip [0..] coords)
  where
    sNum = readScannerNum (head s)
    coords = map readCoord (tail s)

readScannerNum :: String -> Int
readScannerNum s = read $ takeWhile isDigit $ drop (length "--- scanner ") s

readCoord :: String -> V3 Int
readCoord s = V3 x y z
  where
    [x,y,z] = map read $ splitOn "," s
{-
----------------------------------------
-- Graveyard of Failed Attempts
----------------------------------------

-- This was a failed attempt to fix the 'zip unknownKeys knownKeys' bug, which
-- made 'stepScannerMap' fail to solve all scanners. Based on wording in one
-- part of the example, I thought the puzzle was saying you might have to solve
-- some scanners against all known beacons at once (I missed where it says every
-- one can be solved by analyzing scanners pairwise). The worst part was that
-- this solution works and gives the correct answer, but is prohibitively
-- inefficient to solve the full puzzle.
stepScannerDiff
  :: (Map.Map Int (M33 Int, V3 Int), Set.Set (V3 Int), ScannerMap)
  -> (Map.Map Int (M33 Int, V3 Int), Set.Set (V3 Int), ScannerMap)
stepScannerDiff (knownScanners, knownCoords, sm) =
  let (k1, (r, t, ol)) = head $ mapMaybe matchUnknown $ Map.toList sm
      matchUnknown (k1, s) = case matchScanner knownCoords s of
        Nothing -> Nothing
        Just result -> Just (k1, result)
      known' = Map.insert k1 (r, t) knownScanners
      newCoords = Map.elems $ Map.map (\c -> r !* c ^+^ t) $ sm Map.! k1
      knownCoords' = foldl' (flip Set.insert) knownCoords newCoords
      sm' = Map.delete k1 sm
  in (known', knownCoords', sm')

-- Part of above failed attempt.
matchScanner :: Set.Set (V3 Int) -> CoordMap -> Maybe (M33 Int, V3 Int, Overlap)
matchScanner cset cs1 =
  let cs0 = Map.fromList $ zip [0..] $ Set.toList cset
      cs0Diffs = diffs cs0
      cs1Diffs = diffs cs1
      cs1Diffs' = refineDiffsBy cs0Diffs cs1Diffs
      cs1OrientDiffs = map (\o -> (o, Map.map (o !*) cs1Diffs')) orients
      overlaps = map (diffOverlap cs0Diffs <$>) cs1OrientDiffs
  in do
    guard (Map.size cs1Diffs' >= 66)
    (r, ol) <- find ((>= 66) . length . snd) $ overlaps
    let ((c0,_), (c1,_), _) = head ol
        t = cs0 Map.! c0 ^-^ r !* cs1 Map.! c1
    pure (r, t, ol)

-- Different way of permuting orientations that I ended up not using.
permuteOrientations :: V3 Int -> [V3 Int]
permuteOrientations (V3 x y z) = do
  [px,py,pz] <- permutations [x,y,z]
  applyFlipsV (V3 px py pz)
  where
    applyFlipsV v3 = map (v3 *) flips
    flips = V3 <$> [-1,1] <*> [-1,1] <*> [-1,1]
-}
