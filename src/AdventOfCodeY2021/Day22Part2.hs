module AdventOfCodeY2021.Day22Part2 where

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

runF = "data/day22/input.txt"
testF = "data/day22/test2.txt"

load :: FilePath -> IO [Step]
load filePath = do
  strs <- readInputs filePath id
  pure (map readStep strs)

runWith :: FilePath -> IO ()
runWith filePath = do
  steps <- load filePath
  let final = doAll steps
      --part1 = Map.size $ Map.filter id final
      part2 = sum $ map (\(b,r) -> if b then rangeSize3 r else -rangeSize3 r) final
  print part2

type State = [(Bool, V3 Range)]

rangeSize :: Range -> Int
rangeSize (a,b) | a <= b    = b-a+1
                | otherwise = 0

rangeSize3 :: V3 Range -> Int
rangeSize3 (V3 xr yr zr) = rangeSize xr * rangeSize yr * rangeSize zr

initState :: State
initState = []

expandRange :: V3 Range -> [V3 Int]
expandRange (V3 (xa,xb) (ya,yb) (za,zb)) =
  V3 <$> [xa..xb] <*> [ya..yb] <*> [za..zb]

intRange3 :: V3 Range -> V3 Range -> V3 Range
intRange3 (V3 x0 y0 z0) (V3 x1 y1 z1) =
  V3 (x0 `intRange` x1) (y0 `intRange` y1) (z0 `intRange` z1)

intRange :: Range -> Range -> Range
intRange (xa, xb) (ya, yb) = (max xa ya, min xb yb)

limit :: V3 Range
limit = V3 (-50,50) (-50,50) (-50,50)

doStep :: Step -> State -> State
doStep (b, r) s0 =
  let f (b', r') = (not b', r `intRange3` r')
      newRs = filter ((/= 0) . rangeSize3 . snd) $ map f $ s0
      -- ^ had this as (== 0) at first, causing it to hang :(
  in  if b
      then (True, r) : newRs ++ s0
      else newRs ++ s0

doAll :: [Step] -> State
doAll steps = foldl' (flip doStep) initState steps

type Range = (Int, Int)
type Step = (Bool, V3 Range)
readStep :: String -> Step
readStep s =
  let [b, cs] = splitOn " " s
      [x,y,z] = map readRange $ splitOn "," cs
  in  (readB b, V3 x y z)

readB :: String -> Bool
readB "on" = True
readB "off" = False

readRange :: String -> Range
readRange s =
  let [_,r] = splitOn "=" s
      [a,b] = splitOn ".." r
  in  (read a, read b)
