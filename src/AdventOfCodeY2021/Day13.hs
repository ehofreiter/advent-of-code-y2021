module AdventOfCodeY2021.Day13 where

import Control.Lens
import Data.Char
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

run = runWith "data/day13/input.txt"
test = runWith "data/day13/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let [dotStrs, foldStrs] = splitOn [""] strs
      dots = readDots dotStrs
      folds = readFolds foldStrs
      f = head folds
      dots' = foldIt f dots
      c = length dots'
      dots'' = foldl' (flip foldIt) dots folds
      dm = mkMap dots''
  mapM_ print dm

mkMap :: [(Int, Int)] -> [String]
mkMap dots =
  let sizeX = maximum (map fst dots)
      sizeY = maximum (map snd dots)
      coords = [ [(x,y) | x <- [0..sizeX]] | y <- [0..sizeY]]
  in  (map.map) (\c -> if c `elem` dots then '#' else '.') coords

foldIt :: (Axis, Int) -> [(Int, Int)] -> [(Int, Int)]
foldIt (X, n) = nub . foldX n
foldIt (Y, n) = nub . foldY n

foldY :: Int -> [(Int, Int)] -> [(Int, Int)]
foldY n ds = map f ds
  where
    f (x, y) =
      if y <= n
      then (x, y)
      else (x, n - (y - n))

foldX :: Int -> [(Int, Int)] -> [(Int, Int)]
foldX n ds = map f ds
  where
    f (x, y) =
      if x <= n
      then (x, y)
      else (n - (x - n), y)

readDots :: [String] -> [(Int, Int)]
readDots = map readDot

readDot :: String -> (Int, Int)
readDot s =
  let [x, y] = splitOn "," s
  in  (read x, read y)

data Axis = X | Y
  deriving (Eq, Ord, Show)

readFolds :: [String] -> [(Axis, Int)]
readFolds = map readFold

readFold :: String -> (Axis, Int)
readFold s =
  let [a, n] = splitOn "=" s
      axis = case last a of
        'x' -> X
        'y' -> Y
  in  (axis, read n)
