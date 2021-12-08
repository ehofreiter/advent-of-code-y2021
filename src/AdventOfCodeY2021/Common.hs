module AdventOfCodeY2021.Common where

import Data.List.Split

readInputs :: FilePath -> (String -> a) -> IO [a]
readInputs filePath f = do
  fileContents <- readFile filePath
  pure (f <$> lines fileContents)

readIntList :: String -> [Int]
readIntList = map read . splitOn ","
