{-# LANGUAGE TupleSections #-}
-- | Refined version of Day 18, written with no time pressure.
-- Used a zipper to simplify the 'explode' operation. Efficiency seemed about
-- the same, but the code is probably easier to read.
module AdventOfCodeY2021.Day18Refined where

import Control.Applicative
import Data.List
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

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
  print m -- part 1
  print m2 -- part 2

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = map (x,) xs ++ map (,x) xs ++ allPairs xs

magnitude :: N -> Int
magnitude n = case n of
  I i -> i
  P x y -> 3*magnitude x + 2*magnitude y

addN :: N -> N -> N
addN x y = reduce $ P x y

reduce :: N -> N
reduce n = maybe n reduce $ explode n <|> splitN n

data DirZ = LZ N | RZ N
  deriving (Eq, Ord, Show)

type Zipper = [DirZ]

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

explode :: N -> Maybe N
explode = loop 0 []
  where
    loop :: Int -> Zipper -> N -> Maybe N
    loop depth dirs n = case n of
      I i -> Nothing
      P (I x) (I y) ->
        if depth >= 4
        then Just $ update dirs x y (I 0)
        else Nothing
      P x y -> loop (depth+1) (RZ y:dirs) x
           <|> loop (depth+1) (LZ x:dirs) y
    update dirs ux uy n = case dirs of
      LZ x:dirs' ->
        let x' = if ux == 0 then x else updateLastInt (+ ux) x
        in  update dirs' 0 uy $ P x' n
      RZ y:dirs' ->
        let y' = if uy == 0 then y else updateFirstInt (+ uy) y
        in  update dirs' ux 0 $ P n y'
      [] -> n

updateLastInt :: (Int -> Int) -> N -> N
updateLastInt f n = case n of
  I i -> I (f i)
  P x y -> P x (updateLastInt f y)

updateFirstInt :: (Int -> Int) -> N -> N
updateFirstInt f n = case n of
  I i -> I (f i)
  P x y -> P (updateFirstInt f x) y

data N = P N N | I Int
  deriving (Eq, Ord, Show)

readN :: String -> N
readN s = let Right n = P.parse pairParser "" s in n

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
