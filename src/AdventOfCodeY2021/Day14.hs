module AdventOfCodeY2021.Day14 where

import Control.Lens
import Data.Char
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

run = runWith "data/day14/input.txt"
test = runWith "data/day14/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let template = head strs
      rules = map readRule $ drop 2 strs
      ruleMap = mkRuleMap rules
      steps = take 41 $ iterate' (step2 ruleMap) (mkTemplate template)
      final = steps !! 40
      cs = snd final
      most = maximum $ Map.elems cs
      least = minimum $ Map.elems cs
  print (most - least)

counts :: String -> Map.Map Char Int
counts s = foldl' f Map.empty s
  where
    f m c = Map.insertWith (+) c 1 m

mkTemplate :: String -> Template
mkTemplate s = (x, counts s)
  where
    x = foldl' f Map.empty $ mkPairs s
    f m c = Map.insertWith (+) c 1 m

type Template = (Map.Map (Char,Char) Int, Map.Map Char Int)

step2 :: RM -> Template -> Template
step2 rm temp@(t,cs) =
  foldl' f temp $ Map.toList t
  where
    f (t', cs') ((a,b), i) = case Map.lookup (a,b) rm of
      Nothing -> (t', cs')
      Just c -> (Map.insertWith (+) (a,c) i
                 . Map.insertWith (+) (c,b) i
                 -- Originally had (-) here, but found out insertWith f _ v'
                 -- does f v' v (where v is the existing value). I thought
                 -- it was like f v v'. Took me a while to figure it out.
                 . Map.insertWith subtract (a,b) i $ t'
                ,Map.insertWith (+) c i cs')

step :: RM -> String -> String
step rm s =
  let initPairs = mkPairs s
      resultPairs = map (applyRule rm) initPairs
  in  head s : catMaybes (concatMap f resultPairs)
  where
    f (r, b) = [r, Just b]

applyRule :: RM -> (Char,Char) -> (Maybe Char, Char)
applyRule rm (a,b) = (Map.lookup (a,b) rm, b)

mkPairs :: String -> [(Char,Char)]
mkPairs s = zip s (tail s)

type RM = Map.Map (Char,Char) Char

mkRuleMap :: [Rule] -> RM
mkRuleMap = Map.fromList

type Rule = ((Char, Char), Char)

readRule :: String -> Rule
readRule s =
  let [[a,b], [r]] = splitOn " -> " s
  in  ((a,b), r)
