module AdventOfCodeY2021.Day3
  (
  ) where

import Data.List

runDay3 = run "data/day3/input.txt"

run :: FilePath -> IO Int
run filePath = do
  bitM <- readInputs filePath readBits
  let bitMT = transpose bitM
      bitCounts = [compare (length (filter id bs)) (length (filter not bs)) | bs <- bitMT]
      bitsF = map (\o -> case o of
                      GT -> True
                      EQ -> error "same 1s and 0s"
                      LT -> False) bitCounts
      gamma = sum $ zipWith p (reverse bitsF) [0..]
      epsilon = sum $ zipWith p (reverse $ map not bitsF) [0..]
  pure (gamma * epsilon)
  where
    p True i = 2^i
    p False i = 0

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
