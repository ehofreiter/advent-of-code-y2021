{-# LANGUAGE TupleSections #-}
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
      (scannerPositions, sm') = stepAll scannerMap
      part1 = Set.size $ Set.fromList $ concatMap Map.elems (Map.elems sm')
      part2 = maximum $ map (uncurry manhattanDist) $ allPairs scannerPositions
  --pure scannerMap
  --print part1
  print part2

manhattanDist :: V3 Int -> V3 Int -> Int
manhattanDist u v = sum $ abs <$> u - v

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = map (x,) xs ++ allPairs xs

-- stepAll :: ScannerMap -> Set.Set (V3 Int)
-- stepAll sm =
--   loop initKnown initKnownCoords initSm
--   where
--     initKnown = Map.singleton 0 (identity, zero)
--     initKnownCoords = Set.fromList $ Map.elems $ sm Map.! 0
--     initDiffs = diffs (sm Map.! 0)
--     initSm = Map.delete 0 sm
--     loop known knownCoords sm =
--       if Map.size sm == 0
--       then knownCoords
--       else let (known', knownCoords', sm') = stepScanner (known, knownCoords, sm)
--            in  loop known' knownCoords' sm'

stepAll :: ScannerMap -> ([V3 Int], ScannerMap)
stepAll =
  loop initKnown
  where
    initKnown = Map.singleton 0 (identity, zero)
    loop known sm =
      if Map.size known == Map.size sm
      then (map snd $ Map.elems known, sm)
      else let (known', sm') = stepScannerMap (known, sm)
           in  loop known' sm'

-- stepScannerDiff
--   :: (Map.Map Int (M33 Int, V3 Int), Set.Set (V3 Int), ScannerMap)
--   -> (Map.Map Int (M33 Int, V3 Int), Set.Set (V3 Int), ScannerMap)
-- stepScannerDiff (knownScanners, knownCoords, knownDiffs, sm) =
--   let (k1, (r, t, ol)) = head $ mapMaybe matchUnknown $ Map.toList sm
--       matchUnknown (k1, s) = case matchScanner knownCoords s of
--         Nothing -> Nothing
--         Just result -> Just (k1, result)
--       known' = Map.insert k1 (r, t) knownScanners
--       newCoords = Map.elems $ Map.map (\c -> r !* c ^+^ t) $ sm Map.! k1
--       knownCoords' = foldl' (flip Set.insert) knownCoords newCoords
--       sm' = Map.delete k1 sm
--   in (known', knownCoords', sm')

stepScanner
  :: (Map.Map Int (M33 Int, V3 Int), Set.Set (V3 Int), ScannerMap)
  -> (Map.Map Int (M33 Int, V3 Int), Set.Set (V3 Int), ScannerMap)
stepScanner (knownScanners, knownCoords, sm) =
  let (k1, (r, t, ol)) = head $ mapMaybe matchUnknown $ Map.toList sm
      matchUnknown (k1, s) = case matchScanner knownCoords s of
        Nothing -> Nothing
        Just result -> Just (k1, result)
      known' = Map.insert k1 (r, t) knownScanners
      newCoords = Map.elems $ Map.map (\c -> r !* c ^+^ t) $ sm Map.! k1
      knownCoords' = foldl' (flip Set.insert) knownCoords newCoords
      sm' = Map.delete k1 sm
  in (known', knownCoords', sm')

stepScannerMap
  :: (Map.Map Int (M33 Int, V3 Int), ScannerMap)
  -> (Map.Map Int (M33 Int, V3 Int), ScannerMap)
stepScannerMap (knownScanners, sm) =
  let knownKeys = Map.keys knownScanners
      unknownKeys = Map.keys sm \\ knownKeys
      searchPairs = (,) <$> knownKeys <*> unknownKeys
      matchKeys k0 k1 = case matchScanners (sm Map.! k0) (sm Map.! k1) of
        Nothing -> Nothing
        Just result -> Just (k1, result)
      (k1, (r, t, ol)) = head $ mapMaybe (uncurry matchKeys) searchPairs
      known' = Map.insert k1 (r, t) knownScanners
      sm' = Map.adjust (Map.map (\c -> r !* c ^+^ t)) k1 sm
  in (known', sm')

type CoordMap = Map.Map Int (V3 Int)
type DiffMap = Map.Map (Int, Int) (V3 Int)
type ScannerMap = Map.Map Int CoordMap

matchScanners :: CoordMap -> CoordMap -> Maybe (M33 Int, V3 Int, Overlap)
matchScanners cs0 cs1 =
  let cs0Diffs = diffs cs0
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

orients :: [M33 Int]
orients = filter ((== 1) . det33) allOrients
  where
    allOrients = do
      let cols = [V3 1 0 0, V3 0 1 0, V3 0 0 1]
      [vx, vy, vz] <- permutations cols
      applyFlips (V3 vx vy vz)

-- permuteOrientations :: V3 Int -> [V3 Int]
-- permuteOrientations (V3 x y z) = do
--   [px,py,pz] <- permutations [x,y,z]
--   applyFlipsV (V3 px py pz)

applyFlips v3 = map (v3 ^*) flips
applyFlipsV v3 = map (v3 *) flips
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
