module AdventOfCodeY2021.Day3Part2
  (
  ) where

import Data.List
import Data.Foldable
import Data.Traversable

runDay3 = run2 "data/day3/input.txt"

run2 :: FilePath -> IO Int
run2 filePath = do
  llbit <- readInputs filePath readBits
  let oxy = refineBy refineOxy2 llbit
      co2 = refineBy refineCo22 llbit
      o = bitsToInt $ oxy
      c = bitsToInt $ co2
  print o
  print c
  pure (o*c)

bitsToInt :: [Bool] -> Int
bitsToInt bits = sum $ zipWith p (reverse bits) [0..]
  where
    p True i = 2^i
    p False i = 0

testBits :: [String]
testBits =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]

refineOxy2 :: (Int, [[Bool]]) -> Either [Bool] (Int, [[Bool]])
refineOxy2 (i, [lbit]) = Left lbit
refineOxy2 (i, llbit) = Right (i+1, filter f llbit)
  where
    llbit' = filter f llbit
    f lbit = lbit !! i == moreCommon
    moreCommon = length (filter id ithBits) >= length (filter not ithBits)
    ithBits = map (!! i) llbit

refineBy
  :: ((Int, [[Bool]]) -> Either [Bool] (Int, [[Bool]]))
  -> [[Bool]]
  -> [Bool]
refineBy refiner = loop 0
 where
   loop i llbit = case refiner (i, llbit) of
     Left lbit -> lbit
     Right (i', llbit') -> loop i' llbit'

refineCo22 :: (Int, [[Bool]]) -> Either [Bool] (Int, [[Bool]])
refineCo22 (i, [lbit]) = Left lbit
refineCo22 (i, llbit) = Right (i+1, filter f llbit)
  where
    llbit' = filter f llbit
    f lbit = lbit !! i == lessCommon
    lessCommon = length (filter id ithBits) < length (filter not ithBits)
    ithBits = map (!! i) llbit

--
-- First failed attempt
--

-- This was so close to working, but initially had `ints = [0..maxI]`, which
-- applies the refinements in the wrong order, looking at the last bit first.
-- I realized this, and tried to fix it with `ints = [maxI..0]`, which makes
-- `ints` just the empty list `[]`. I saw it wasn't filtering anything, and
-- thought it was a bug in `refineOxy` and `refineCo2`. Finally I gave up
-- and wrote the versions above. As you can see below, all I really had to
-- do was change it to `ints = reverse [0..maxI]`. That makes it work. :(

run :: FilePath -> IO Int
run filePath = do
  llbit <- readInputs filePath readBits
  let maxI = length (head llbit) - 1
      ints = reverse [0..maxI]
      refineOxys = map refineOxy ints
      oxy = foldl' (.) id refineOxys llbit
      refineCo2s = map refineCo2 ints
      co2 = foldl' (.) id refineCo2s llbit
      o = bitsToInt $ head oxy
      c = bitsToInt $ head co2
  --print maxI
  --print ints
  --traverse_ print $ (map.map) toBitChar co2
  print o
  print c
  pure (o*c)

refineOxy :: Int -> [[Bool]] -> [[Bool]]
refineOxy _ [lbit] = [lbit]
refineOxy i llbit = filter f llbit
  where
    f lbit = lbit !! i == moreCommon
    moreCommon = length (filter id ithBits) >= length (filter not ithBits)
    ithBits = map (!! i) llbit

refineCo2 :: Int -> [[Bool]] -> [[Bool]]
refineCo2 _ [lbit] = [lbit]
refineCo2 i llbit = filter f llbit
  where
    f lbit = lbit !! i == lessCommon
    lessCommon = length (filter id ithBits) < length (filter not ithBits)
    ithBits = map (!! i) llbit

readBits :: String -> [Bool]
readBits = map toBit

toBit :: Char -> Bool
toBit '0' = False
toBit '1' = True

toBitChar :: Bool -> Char
toBitChar False = '0'
toBitChar True = '1'

readInputs :: FilePath -> (String -> a) -> IO [a]
readInputs filePath f = do
  fileContents <- readFile filePath
  pure (f <$> lines fileContents)
