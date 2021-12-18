{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2021.Day18 where

import Control.Applicative
import Control.Lens
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

run = runWith "data/day18/input.txt"
test = runWith "data/day18/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let ns = map readN strs
      finalN = foldl1' addN ns
      m = magnitude finalN
      ps = allPairs ns
      m2 = maximum $ map (magnitude . uncurry addN) ps
  --print m -- part 1
  print m2 -- part 2

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = map (x,) xs ++ map (,x) xs ++ allPairs xs

readN :: String -> N
readN s = let Right n = P.parse pairParser "" s in n

magnitude :: N -> Int
magnitude n = case n of
  I i -> i
  P x y -> 3*magnitude x + 2*magnitude y

addN :: N -> N -> N
addN x y = reduce $ P x y

reduce :: N -> N
reduce n = case explode n of
  Just n' -> reduce n'
  Nothing -> case splitN n of
    Just n' -> reduce n'
    Nothing -> n

data Dir = L | R
  deriving (Eq, Ord, Show)

splitN :: N -> Maybe N
splitN n = case n of
  I i | i >= 10 -> Just $ P (I $ floor $ half i) (I $ ceiling $ half i)
      | otherwise -> Nothing
  P x y -> case splitN x of
    Just x' -> Just $ P x' y
    Nothing -> case splitN y of
      Just y' -> Just $ P x y'
      Nothing -> Nothing
  where
    half i = fromIntegral i / 2

findExplodePair :: N -> Maybe ([Dir], Int, Int)
findExplodePair = loop 0 []
  where
    loop :: Int -> [Dir] -> N -> Maybe ([Dir], Int, Int)
    loop depth dirs n =
      case n of
        I i -> Nothing
        P (I x) (I y) ->
          if depth >= 4
          then Just (reverse dirs, x, y)
          else Nothing
        P x y -> loop (depth+1) (L:dirs) x
             <|> loop (depth+1) (R:dirs) y

updateIntAt :: [Dir] -> (Int -> Int) -> N -> N
updateIntAt d f n = case (d, n) of
  (_, I i) -> I (f i)
  (L:dirs, P x y) -> P (updateIntAt dirs f x) y
  (R:dirs, P x y) -> P x (updateIntAt dirs f y)
  ([], P _ _) -> error "unexpected pair"

locateLeftOf :: [Dir] -> Maybe [Dir]
locateLeftOf dirs =
  let rights = filter ((== R) . snd) $ zip [0..] dirs
  in  case rights of
        [] -> Nothing
        rs -> let lastRight = fst $ last rs
              in  Just $ take lastRight dirs ++ (L:repeat R)

locateRightOf :: [Dir] -> Maybe [Dir]
locateRightOf dirs =
  let lefts = filter ((== L) . snd) $ zip [0..] dirs
  in  case lefts of
        [] -> Nothing
        ls -> let lastLeft = fst $ last ls
              in  Just $ take lastLeft dirs ++ (R:repeat L)

updateLeftOf :: [Dir] -> Int -> N -> N
updateLeftOf dirs u n =
  case locateLeftOf dirs of
    Just ldirs -> updateIntAt ldirs (+ u) n
    Nothing -> n

updateRightOf :: [Dir] -> Int -> N -> N
updateRightOf dirs u n =
  case locateRightOf dirs of
    Just rdirs -> updateIntAt rdirs (+ u) n
    Nothing -> n

explode :: N -> Maybe N
explode n =
  case findExplodePair n of
    Nothing -> Nothing
    Just (dirs, x, y) -> Just
                       . updateLeftOf dirs x
                       . updateRightOf dirs y
                       . zeroPair dirs
                       $ n

zeroPair :: [Dir] -> N -> N
zeroPair dirs n = case (dirs, n) of
  ([], _) -> I 0
  (L:dirs', P x y) -> P (zeroPair dirs' x) y
  (R:dirs', P x y) -> P x (zeroPair dirs' y)

data N = P N N | I Int
  deriving (Eq, Ord, Show)

pairParser :: P.Parsec String () N
pairParser = do
  P.char '['
  a <- numParser
  P.char ','
  b <- numParser
  P.char ']'
  pure (P a b)

intParser :: P.Parsec String () N
intParser = I . read <$> P.many1 P.digit

numParser :: P.Parsec String () N
numParser = pairParser <|> intParser
