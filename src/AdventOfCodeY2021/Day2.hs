module AdventOfCodeY2021.Day2 where

import Data.Foldable

type Pos = (Int, Int)

run :: FilePath -> IO Int
run filePath = do
  motions <- readMotions filePath
  let (x, y) = sumMotions motions
  pure (x * y)

sumMotions :: [Pos] -> Pos
sumMotions = foldl' addPos (0, 0)

addPos :: Pos -> Pos -> Pos
addPos (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

readMotions :: FilePath -> IO [Pos]
readMotions filePath = do
  fileContents <- readFile filePath
  pure (readMotion <$> lines fileContents)

readMotion :: String -> Pos
readMotion str =
  let [motion, xstr] = words str
      x = read xstr :: Int
  in case motion of
    "forward" -> (x, 0)
    "down" -> (0, x)
    "up" -> (0, -x)

--
-- Part 2
--

-- (aim, horizontal pos, depth)
type State = (Int, Int, Int)

data Motion
  = Down Int
  | Up Int
  | Forward Int

run2 :: FilePath -> IO Int
run2 filePath = do
  motions <- readMotions2 filePath
  let (aim, h, d) = foldMotions motions
  pure (h * d)

readMotions2 :: FilePath -> IO [Motion]
readMotions2 filePath = do
  fileContents <- readFile filePath
  pure (readMotion2 <$> lines fileContents)

readMotion2 :: String -> Motion
readMotion2 str =
  let [motion, xstr] = words str
      x = read xstr :: Int
  in case motion of
    "forward" -> Forward x
    "down" -> Down x
    "up" -> Up x

foldMotions :: [Motion] -> State
foldMotions = foldl' updateState (0, 0, 0)

updateState :: State -> Motion -> State
updateState (aim, h, d) motion =
  case motion of
    Down x -> (aim + x, h, d)
    Up x -> (aim - x, h, d)
    Forward x -> (aim, h + x, d + x*aim)
