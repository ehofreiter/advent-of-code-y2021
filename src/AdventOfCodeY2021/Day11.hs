module AdventOfCodeY2021.Day11 where

import Control.Lens
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

run = runWith "data/day11/input.txt"
test = runWith "data/day11/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let em = readMap strs
      (fs, em') = step em 100 0
      (steps, em'') = step2 em 0
  mapM_ print $ emToString em'
  print fs -- part 1
  print (steps + 1) -- part 2

emToString :: EM -> [String]
emToString em = transpose $ chunksOf 10 $ concatMap show $ Map.elems em

type Coord = (Int, Int)
type EM = Map.Map Coord Int

readMap :: [String] -> EM
readMap ss = foldl' Map.union Map.empty ems
  where
    ems = zipWith readLine [0..] ss

readLine :: Int -> String -> EM
readLine y es = Map.fromList $ zipWith (\e x -> ((x,y), read [e])) es [0..]

adj :: EM -> Coord -> [Coord]
adj em (x,y) = [(x+x',y+y') | x' <- [-1,0,1], y' <- [-1,0,1]] `intersect` Map.keys em

step2 :: EM -> Int -> (Int, EM)
step2 em steps =
  let em' = Map.map succ em
      em'' = stepFlash em' []
      em''' = reset em''
  in  if allZero em'''
         then (steps, em''')
         else step2 em''' (steps + 1)
  where
    allZero x = all (== 0) $ Map.elems x

step :: EM -> Int -> Int -> (Int, EM)
step em steps flashes =
  if steps == 0
  then (flashes, em)
  else
    let em' = Map.map succ em
        em'' = stepFlash em' []
        em''' = reset em''
    in  step em''' (steps - 1) (flashes + (length $ getFlashCoords em''))

reset :: EM -> EM
reset em = Map.map f em
  where
    f e = if e > 9 then 0 else e

getFlashCoords :: EM -> [Coord]
getFlashCoords = Map.keys . Map.filter (> 9)

stepFlash :: EM -> [Coord] -> EM
stepFlash em alreadyFlashed =
  let allFlashes = getFlashCoords em
      flashCoords = allFlashes \\ alreadyFlashed
  in  if null flashCoords
      then em
      -- Had a tricky bug here. For some reason I initially applied
      -- 'nub' to the adjs here. Like:
      --   let adjs = (nub $ concatMap (adj em) flashCoords) \\ allFlashes
      -- Took me a while to find it!
      else let adjs = (concatMap (adj em) flashCoords) \\ allFlashes
               em' = foldl' (flip $ Map.adjust succ) em adjs
           in  stepFlash em' allFlashes
