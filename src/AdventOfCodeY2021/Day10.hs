module AdventOfCodeY2021.Day10 where

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

run = runWith "data/day10/input.txt"
test = runWith "data/day10/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let parses = map (flip step []) strs
      --s = sum $ map score parses
      getRight x = case x of
        Left _ -> Nothing
        Right y -> Just y
      incompletes = mapMaybe (getRight . flip step []) strs
      s2 = map scoreLine incompletes
      smid = sort s2 !! ((length s2 - 1) `div` 2)
  print smid --p2
  --print s -- p1

scoreLine :: [ChunkType] -> Int
scoreLine = foldl' f 0
  where
    f i c = 5*i + score2 c

score :: Either Err () -> Int
score (Right _) = 0
score (Left (Err _ Round)) = 3
score (Left (Err _ Square)) = 57
score (Left (Err _ Curly)) = 1197
score (Left (Err _ Angle)) = 25137

score2 :: ChunkType -> Int
score2 Round = 1
score2 Square = 2
score2 Curly = 3
score2 Angle = 4

step :: String -> [ChunkType] -> Either Err [ChunkType]
step [] cts = Right cts
step (c:cs) [] = step cs [snd (readToken c)]
step (c:cs) (ct:cts) =
  if isOpen
  then step cs (chunkType:ct:cts)
  else
    if chunkType == ct
    then step cs cts
    else Left (Err ct chunkType)
  where
    (isOpen, chunkType) = readToken c

readToken :: Char -> (Bool, ChunkType)
readToken '(' = (True, Round)
readToken '[' = (True, Square)
readToken '{' = (True, Curly)
readToken '<' = (True, Angle)
readToken ')' = (False, Round)
readToken ']' = (False, Square)
readToken '}' = (False, Curly)
readToken '>' = (False, Angle)

data Err = Err ChunkType ChunkType --expected found
  deriving (Eq, Ord, Show)

opens :: String
opens = "([{<"

closes :: String
closes = ")]}>"

data ChunkType = Round | Square | Curly | Angle
  deriving (Eq, Ord, Show)

chunkChars :: ChunkType -> (Char, Char)
chunkChars Round = ('(',')')
chunkChars Square = ('[',']')
chunkChars Curly = ('{','}')
chunkChars Angle = ('<','>')
