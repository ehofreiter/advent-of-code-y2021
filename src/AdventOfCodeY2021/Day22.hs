module AdventOfCodeY2021.Day22 where

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
testF = "data/day22/test.txt"

-- run = runWith 
-- test = runWith 

load :: FilePath -> IO [Step]
load filePath = do
  strs <- readInputs filePath id
  pure (map readStep strs)

runWith :: FilePath -> IO ()
runWith filePath = do
  steps <- load filePath
  let final = doAll steps
      part1 = Map.size $ Map.filter id final
  print part1

type State = Map.Map (V3 Int) Bool

initState :: State
initState =
  let coords = V3 <$> [-50..50] <*> [-50..50] <*> [-50..50]
  in  Map.fromList $ map (\c -> (c, False)) coords

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
doStep (b, V3 xr yr zr) s0 =
  let coords = expandRange (V3 xr yr zr `intRange3` limit)
  in  foldl' (\m c -> Map.insert c b m) s0 coords

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
