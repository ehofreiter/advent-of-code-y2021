module AdventOfCodeY2021.Day1
  ( Depth(..)
  , Diff(..)
  , runPart1
  ) where

import Control.Applicative (ZipList(..))

--
-- Part 1
--

runPart1 :: FilePath -> IO Int
runPart1 filePath = do
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

--
-- Part 2
--

runPart2 :: FilePath -> IO Int
runPart2 filePath = do
  depths <- readDepths filePath
  let wds = windowDepth <$> applyWindow depths
  pure (countIncreases $ diffs wds)

data Window = Window Depth Depth Depth
  deriving Show

windowDepth :: Window -> Depth
windowDepth (Window (Depth x) (Depth y) (Depth z)) = Depth (x + y + z)

-- >>> applyWindow . fmap Depth $ [10, 15, 3, 15]
-- [Window (Depth {unDepth = 3}) (Depth {unDepth = 15}) (Depth {unDepth = 10}),Window (Depth {unDepth = 15}) (Depth {unDepth = 3}) (Depth {unDepth = 15})]
applyWindow :: [Depth] -> [Window]
applyWindow depths = case depths of
  [] -> []
  [x] -> []
  [x,y] -> []
  xs -> getZipList
    $   Window
    <$> ZipList xs
    <*> ZipList (tail xs)
    <*> ZipList (tail $ tail xs)
