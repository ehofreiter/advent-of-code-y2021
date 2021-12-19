{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2021.Common where

import           Data.Foldable
import           Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq((:|>), (:<|)), (><))
import qualified Data.Set as Set

readInputs :: FilePath -> (String -> a) -> IO [a]
readInputs filePath f = do
  fileContents <- readFile filePath
  pure (f <$> lines fileContents)

readIntList :: String -> [Int]
readIntList = map read . splitOn ","

-- | Creates a histogram from a list of values, mapping the number of
-- occurrences of each value in the given list.
-- >>> mkHistogram "NNNCNCCHHNNNNH"
-- fromList [('C',3),('H',3),('N',8)]
-- >>> mkHistogram []
-- fromList []
mkHistogram :: (Ord a, Foldable t) => t a -> Map.Map a Int
mkHistogram = foldl' (\m x -> Map.insertWith (+) x 1 m) Map.empty

-- | Flattens the histogram into a list. Right identity of mkHistogram.
-- >>> unMkHistogram $ mkHistogram "NNNCNCCHHNNNNH"
-- "CCCHHHNNNNNNNN"
-- >>> mkHistogram $ unMkHistogram $ Map.fromList [('A',3),('B',2),('C',1)]
-- fromList [('A',3),('B',2),('C',1)]
unMkHistogram :: Map.Map a Int -> [a]
unMkHistogram = concatMap (\(x, i) -> replicate i x) . Map.toList

-- | Gives all ways to choose 2 items from a list, modulo order.
-- >>> choose2 [4,5,2,8]
-- [(4,5),(4,2),(4,8),(5,2),(5,8),(2,8)]
-- >>> choose2 [1,1,1]
-- [(1,1),(1,1),(1,1)]
-- >>> map (\n -> ((length (choose2 [1..n]), n*(n-1)`div`2))) [0..10]
-- [(0,0),(0,0),(1,1),(3,3),(6,6),(10,10),(15,15),(21,21),(28,28),(36,36),(45,45)]
choose2 :: [a] -> [(a,a)]
choose2 [] = []
choose2 (x:xs) = map (x,) xs ++ choose2 xs

bfs :: Ord n => (n -> [n]) -> Seq n -> Seq n
bfs getAdjs initialQueue = loop initialSeen initialQueue Seq.empty
  where
    initialSeen = Set.fromList $ toList initialQueue
    loop seen queue result =
      case queue of
        Seq.Empty -> result
        n :<| ns ->
          let newNodes = filter (`Set.notMember` seen) $ getAdjs n
              queue' = ns >< Seq.fromList newNodes
              seen' = seen `Set.union` Set.fromList newNodes
          in  loop seen' queue' (result :|> n)
