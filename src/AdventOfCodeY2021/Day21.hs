module AdventOfCodeY2021.Day21 where

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

import AdventOfCodeY2021.CoordMap as CM

runF = (5, 9)
testF = (4, 8)

run = go runF
test = go testF

type State = ((Int, Int), (Int, Int), Bool, Int) -- spaces, scores, p1turn, die
initState p = (p, (0,0), True, 1)

go :: (Int, Int) -> Int
go (p1, p2) = loop $ initState (p1,p2)
  where
    loop state@((p1,p2),(s1,s2),p1turn,d) =
      case winner (s1,s2) of
        Just 1 -> (d-1)*s2
        Just 2 -> (d-1)*s1
        _ -> loop (turn state)

winner :: (Int, Int) -> Maybe Int
winner (s1, s2) | s1 >= 1000 = Just 1
                | s2 >= 1000 = Just 2
                | otherwise  = Nothing

turn :: State -> State
turn ((p1, p2), (s1, s2), p1turn, d) = ((p1', p2'), (s1', s2'), not p1turn, d')
  where
    ds = map dv [d, d+1, d+2]
    d' = d+3
    (p1', s1') = if p1turn then update (p1, s1) ds else (p1, s1)
    (p2', s2') = if not p1turn then update (p2, s2) ds else (p2, s2)

dv :: Int -> Int
dv d = (d-1) `mod` 100 + 1

update :: (Int, Int) -> [Int] -> (Int, Int)
update (p, s) ds =
  let p' = ((p-1) + sum ds) `mod` 10 + 1
      s' = s + p'
  in  (p', s')



load :: FilePath -> IO [String]
load filePath = do
  strs <- readInputs filePath id
  pure strs

runWith :: FilePath -> IO ()
runWith filePath = do
  input <- load filePath
  print input

type Input = (Int, Int) -- P1, P2
