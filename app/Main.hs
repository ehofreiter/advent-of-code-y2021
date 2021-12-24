module Main where

import System.Environment

import AdventOfCodeY2021.Day23

main :: IO ()
main = do
  args <- getArgs
  runWith (head args)
