module AdventOfCodeY2021.Day1
  ( Depth(..)
  , Diff(..)
  , run
  ) where

run :: FilePath -> IO Int
run filePath = do
  depths <- readDepths filePath
  pure (countIncreases $ diffs depths)

readDepths :: FilePath -> IO [Depth]
readDepths filePath = do
  fileContents <- readFile filePath
  pure (Depth . read <$> lines fileContents)

newtype Depth = Depth { unDepth :: Int }
  deriving Show

newtype Diff = Diff { unDiff :: Int }
  deriving Show

diff :: Depth -> Depth -> Diff
diff (Depth x) (Depth y) = Diff (x - y)

-- >>> fmap unDiff . diffs . fmap Depth $ [10, 15, 3, 15]
-- [5,-12,12]
-- >>> diffs []
-- []
-- >>> diffs (Depth <$> [5])
-- []
diffs :: [Depth] -> [Diff]
diffs depths = case depths of
  [] -> []
  xs -> zipWith diff (tail xs) xs

-- >>> countIncreases . fmap Diff $ [0, -1, 1]
-- 1
-- >>> countIncreases . diffs . fmap Depth $ [10, 15, 3, 15]
-- 2
countIncreases :: [Diff] -> Int
countIncreases = length . filter (> 0) . fmap unDiff

