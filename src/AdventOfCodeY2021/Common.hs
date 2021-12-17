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
