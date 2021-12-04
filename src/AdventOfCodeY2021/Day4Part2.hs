module AdventOfCodeY2021.Day4Part2 where

import Data.List
import Data.Foldable
import Data.Traversable

import AdventOfCodeY2021.Common

run = runWith "data/day4/input.txt"
test = runWith "data/day4/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath readAs
  let ns = getNumbers (head strs)
  let boards = getBoards (filter (/= "") $ tail strs)
  print ns
  print boards
  let result = step (ns, map mkMBoard boards)
  print result
  print (score result)

readAs :: String -> String
readAs = id

score :: (Int, MBoard) -> Int
score (n, mb) = n * sum unmarked
  where unmarked = map snd . filter (not . fst) $ concat mb

type MBoard = [[(Bool, Int)]]

mkMBoard :: Board -> MBoard
mkMBoard = (map.map) (\i -> (False, i))

mark :: Int -> MBoard -> MBoard
mark n = (map.map) f
  where
    f (m, i) = if n == i then (True, i) else (m, i)

check :: MBoard -> Bool
check mb = any (all fst) (mb ++ transpose mb)

step :: ([Int],  [MBoard]) -> (Int, MBoard)
step (n:ns, mbs) =
  let mbs' = map (mark n) mbs
      winner = find check mbs'
  in  case winner of
    Just b -> case mbs' of
      [_] -> (n, b)
      _ -> step (ns, filter (not . check) mbs')
    Nothing -> step (ns, mbs')

type Board = [[Int]]

getBoards :: [String] -> [Board]
getBoards [] = []
getBoards strs =
  let (board, strs') = getBoard strs
  in  board : getBoards strs'

getBoard :: [String] -> (Board, [String])
getBoard strs = (b, strs')
  where
    b = (map.map) read . map words . take 5 $ strs
    strs' = drop 5 strs
getNumbers :: String -> [Int]
getNumbers [] = []
getNumbers (',':s) = getNumbers' s
getNumbers s = getNumbers' s

getNumbers' :: String -> [Int]
getNumbers' s =
  let (si, sis) = break (== ',') s
  in  read si : getNumbers sis
