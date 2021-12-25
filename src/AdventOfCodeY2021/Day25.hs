{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2021.Day25 where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Linear
import Data.List.Split
import           Data.Functor.WithIndex
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq((:|>), (:<|)), (><))
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

import AdventOfCodeY2021.CoordVec as CV

runF = "data/day25/input.txt"
testF = "data/day25/test.txt"

load :: FilePath -> IO CucMap
load filePath = do
  strs <- readInputs filePath id
  pure $ fromLists $ (map.map) readCuc strs

part1 :: CucMap -> Int
part1 = loop 0
  where
    loop n cm =
      let cm' = stepCM cm
          n' = n+1
      in  if cm' == cm
          then n'
          else loop n' cm'

printCM :: CucMap -> IO ()
printCM cm =
  mapM_ print $ (map.map) showCuc $ toLists cm

stepCM :: CucMap -> CucMap
stepCM cm =
  let cm' = imap (\c _ -> nextSpotEast cm c) cm
      cm'' = imap (\c _ -> nextSpotSouth cm' c) cm'
  in  cm''

-- stepCM :: CucMap -> CucMap
-- stepCM cm = imap f cm
--   where
--     f c _ = nextSpot cm c

nextSpotEast :: CucMap -> (Int, Int) -> Maybe Cuc
nextSpotEast cm c = case cm CV.! c of
  Nothing -> case cm CV.! westC cm c of
    Just East -> Just East
    _ -> Nothing
  Just East -> case cm CV.! eastC cm c of
    Nothing -> Nothing
    _       -> Just East
  cuc -> cuc

nextSpotSouth :: CucMap -> (Int, Int) -> Maybe Cuc
nextSpotSouth cm c = case cm CV.! c of
  Nothing -> case cm CV.! northC cm c of
    Just South -> Just South
    _ -> Nothing
  Just South -> case cm CV.! southC cm c of
    Nothing -> Nothing
    _       -> Just South
  cuc -> cuc

nextSpot :: CucMap -> (Int, Int) -> Maybe Cuc
nextSpot cm c = case cm CV.! c of
  Nothing -> case (cm CV.! westC cm c, cm CV.! northC cm c) of
    (Just East, _) -> Just East
    (_, Just South) -> Just South
    _ -> Nothing
  Just East -> case cm CV.! eastC cm c of
    Nothing -> Nothing
    _       -> Just East
  Just South -> case (cm CV.! southC cm c, cm CV.! southC cm (westC cm c)) of
    (Nothing, Just East) -> Just South
    (Nothing, _) -> Nothing
    _       -> Just South

eastC :: CucMap -> Coord -> Coord
eastC cm (x,y) = ((x+1) `mod` rowSize cm, y)

westC :: CucMap -> Coord -> Coord
westC cm (x,y) = ((x-1) `mod` rowSize cm, y)

southC :: CucMap -> Coord -> Coord
southC cm (x,y) = (x, (y+1) `mod` colSize cm)

northC :: CucMap -> Coord -> Coord
northC cm (x,y) = (x, (y-1) `mod` colSize cm)

data Cuc = East | South
  deriving (Eq, Ord, Show)

type CucMap = CoordVec (Maybe Cuc)

readCuc :: Char -> Maybe Cuc
readCuc '.' = Nothing
readCuc '>' = Just East
readCuc 'v' = Just South

showCuc :: Maybe Cuc -> Char
showCuc Nothing = '.'
showCuc (Just East) = '>'
showCuc (Just South) = 'v'
