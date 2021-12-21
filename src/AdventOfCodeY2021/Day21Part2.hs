module AdventOfCodeY2021.Day21Part2 where

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

type State = ((Int, Int), (Int, Int), Bool) -- spaces, scores, p1turn, die
initState p = (p, (0,0), True)

add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z

rolls :: Map.Map Int Int
rolls = mkHistogram $ add3 <$> [1,2,3] <*> [1,2,3] <*> [1,2,3]

-- state count
type SC = Map.Map State Int

initSc p = Map.singleton (initState p) 1

go :: (Int, Int) -> (Int, Int)
go (p1, p2) = loop 0 0 $ initSc (p1,p2)
  where
    loop p1c p2c sc =
      if Map.null sc
      then (p1c, p2c)
      else let (p1c', p2c', sc') = turnC (p1c, p2c, sc)
           in  loop p1c' p2c' sc'

winner :: (Int, Int) -> Maybe Int
winner (s1, s2) | s1 >= 21 = Just 1
                | s2 >= 21 = Just 2
                | otherwise  = Nothing

turnC :: (Int, Int, SC) -> (Int, Int, SC)
turnC (p1c, p2c, sc) =
  let scs = Map.toList sc
      scs' = concatMap turnQ scs
      (p1c', p2c', scs'') = splitWinners p1c p2c scs'
      sc' = foldl' (\m (s, c) -> Map.insertWith (+) s c m) Map.empty $ scs''
  in  (p1c', p2c', sc')

splitWinners :: Int -> Int -> [(State, Int)] -> (Int, Int, [(State, Int)])
splitWinners p1 p2 ss = loop ss (p1, p2, [])
  where
    loop [] r = r
    loop ((s@(_, score, _), c):ss) (p1, p2, rss) =
      case winner score of
        Just 1 -> loop ss (p1+c, p2, rss)
        Just 2 -> loop ss (p1, p2+c, rss)
        _      -> loop ss (p1, p2, (s,c):rss)

turnQ :: (State, Int) -> [(State, Int)]
turnQ (((p1, p2), (s1, s2), p1turn), count) =
  let dcs = Map.toList rolls
      f (ds, c) =
        let (p1', s1') = if p1turn then update2 (p1, s1) ds else (p1, s1)
            (p2', s2') = if not p1turn then update2 (p2, s2) ds else (p2, s2)
        in  (((p1',p2'),(s1',s2'),not p1turn), count*c)
  in  map f dcs

update2 :: (Int, Int) -> Int -> (Int, Int)
update2 (p, s) ds =
  let p' = ((p-1) + ds) `mod` 10 + 1
      s' = s + p'
  in  (p', s')

{-
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
-}
