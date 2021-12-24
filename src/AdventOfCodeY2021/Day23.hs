{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2021.Day23 where

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

runF, testF :: Locs
runF = initLocs [C,D,D,B] [B,C,B,C] [D,B,A,A] [D,A,C,A]
testF = initLocs [B,D,D,A] [C,C,B,D] [B,B,A,C] [D,A,C,A]

runWith :: String -> IO ()
runWith "test" = print $ solve (initState testF)
runWith "input" = print $ solve (initState runF)
runWith _       = putStrLn "unknown command: use 'test' or 'input'"

initState :: Locs -> State
initState locs = Map.singleton 0 [(0, locs)]

hall :: [V2 Int]
hall = [V2 x 0 | x <- [0,1,3,5,7,9,10]]

hallSpots :: Locs -> [(Loc, Maybe Color)]
hallSpots locs = map (\l -> (l, locs Map.! l)) hall

wellLocs :: Int -> [Loc]
wellLocs x = [V2 x y | y <- [1..4]]

wellA = wellLocs 2
wellB = wellLocs 4
wellC = wellLocs 6
wellD = wellLocs 8

wellSpots :: Locs -> Int -> [(Loc, Maybe Color)]
wellSpots locs x = map (\l -> (l, locs Map.! l)) (wellLocs x)

data Color = A | B | C | D
  deriving (Eq, Ord, Show)

type Loc = V2 Int

type Locs = Map.Map Loc (Maybe Color)

initLocs :: [Color] -> [Color] -> [Color] -> [Color] -> Locs
initLocs initA initB initC initD = Map.fromList
  $  zip hall (repeat Nothing)
  ++ zip wellA (map Just initA)
  ++ zip wellB (map Just initB)
  ++ zip wellC (map Just initC)
  ++ zip wellD (map Just initD)

showLocs :: Locs -> [String]
showLocs locs =
  [[showSpot (Map.lookup (V2 x y) locs) | x <- [0..10]] | y <- [0..4]]

printLocs :: Locs -> IO ()
printLocs = mapM_ putStrLn . showLocs

showSpot :: Maybe (Maybe Color) -> Char
showSpot Nothing = ' '
showSpot (Just mc) = case mc of
  Nothing -> '.'
  Just A  -> 'A'
  Just B  -> 'B'
  Just C  -> 'C'
  Just D  -> 'D'

stepCost :: Color -> Int
stepCost A = 1
stepCost B = 10
stepCost C = 100
stepCost D = 1000

-- manhattan distance
steps :: Loc -> Loc -> Int
steps a b = sum $ abs <$> a - b

type Node = (Int, Int, Locs) -- waste cost, total cost, board state
type State = Map.Map Int [(Int, Locs)] -- waste cost->(total cost,board states)

solve :: State -> Int
solve state =
  case expandCheapest state of
    Left cost -> cost
    Right state' -> solve state'

solveN :: Int -> State -> Either Int State
solveN n state
  | n <= 0    = Right state
  | otherwise =
    case expandCheapest state of
      Left cost -> Left cost
      Right state' -> solveN (n-1) state'

printMin :: State -> IO ()
printMin s =
  case Map.lookupMin s of
    Nothing -> putStrLn "Empty!"
    Just (w, clocss) -> mapM_ (printNode . uncurry (w,,)) clocss

printState :: State -> IO ()
printState
  = mapM_ printNode
  . concatMap (\(w, cls) -> map (uncurry (w,,)) cls)
  . Map.toList

expandCheapest :: State -> Either Int State
expandCheapest state =
  case Map.minViewWithKey state of
    Just ((waste, costLocStates), state') ->
      case find (winner . snd) costLocStates of
        Just (cost, locs) -> Left cost
        Nothing ->
          let nodes = concatMap (applyAllMoves . uncurry (waste,,)) costLocStates
          in  Right $ foldl' addNode state' nodes
    Nothing ->
      error "halp!"
  where
    addNode st (w, c, ls) = Map.insertWith (++) w [(c,ls)] st

winner :: Locs -> Bool
winner locs =
  all wellFilled [A,B,C,D]
  where
    wellFilled c = all (== Just c) $ map snd $ wellSpots locs $ homeWellX c

applyAllMoves :: Node -> [Node]
applyAllMoves (waste, cost, locs) =
  map (applyMove (waste, cost, locs)) $ allMoves locs

printNode :: Node -> IO ()
printNode (waste, cost, locs) = do
  putStrLn $ "Waste: " <> show waste <> ", Cost: " <> show cost
  printLocs locs

applyMove :: Node -> Move -> Node
applyMove (waste, cost, locs) (lFrom, lTo) =
  let color = fromJust $ locs Map.! lFrom
      cost' = cost + steps lFrom lTo * stepCost color
      waste' = waste + wasteCost color (lFrom, lTo)
      locs' = Map.insert lFrom Nothing $ Map.insert lTo (Just color) locs
  in  (waste', cost', locs')

type Move = (Loc, Loc) -- from, to

allMoves :: Locs -> [Move]
allMoves locs =
  case hallMoves locs of
    (m:_) -> [m] -- ensure hall moves get done in one order first
    []    -> allWellMoves locs

hallMoves :: Locs -> [Move]
hallMoves locs = mapMaybe (tryMoveHome locs) hall

tryMoveHome :: Locs -> Loc -> Maybe Move
tryMoveHome locs loc = do
  c <- locs Map.! loc
  home <- findHomeLoc locs c
  guard (unblockedHall locs (loc, home))
  pure (loc, home)

unblockedHall :: Locs -> Move -> Bool
unblockedHall locs (V2 fromX _, V2 toX _) =
  all ((== Nothing) . snd) $ filter (between . fst) (hallSpots locs)
  where
    between (V2 hx _) = min fromX toX < hx && hx < max fromX toX

homeWellX :: Color -> Int
homeWellX A = 2
homeWellX B = 4
homeWellX C = 6
homeWellX D = 8

findHomeLoc :: Locs -> Color -> Maybe Loc
findHomeLoc locs c =
  case dropHomies $ reverse $ wellSpots locs (homeWellX c) of
    ((loc, Nothing):_) -> Just loc
    _                  -> Nothing
  where
    dropHomies = dropWhile ((== Just c) . snd)

-- How much extra cost is incurred above the most direct route. Optimizing this
-- still gives the right answer, but avoids de-prioritizing paths where
-- expensive but inevitable moves are made early.
wasteCost :: Color -> Move -> Int
wasteCost c (V2 fromX fromY, V2 toX toY)
  | fromY == 0 = 0 -- don't count moves from the hall to home
  | otherwise =
    let homeX = homeWellX c
        -- total X distance this guy will travel in his life
        moveXTravel = abs (toX - fromX) + abs (homeX - toX)
        -- minimal X distance, which is 2 even if homeX = fromX since you can't
        -- stop on the spot in front of a room
        directXTravel = max 2 (abs (homeX - fromX))
        wasteSteps = moveXTravel - directXTravel -- should always be > 0
    in  wasteSteps * stepCost c

wellTop :: Locs -> Int -> Maybe Loc
wellTop locs wx =
  let filled = dropWhile (isNothing . snd) $ wellSpots locs wx
  in  case filled of
    ((l, Just c):filled') ->
      if homeWellX c == wx && all ((== Just c) . snd) filled'
      then Nothing
      else Just l
    _ -> Nothing

wellTops :: Locs -> [Loc]
wellTops locs = mapMaybe (wellTop locs) [2,4,6,8]

allWellMoves :: Locs -> [(Loc, Loc)]
allWellMoves locs = concatMap (wellMoves locs) [2,4,6,8]

wellMoves :: Locs -> Int -> [(Loc, Loc)]
wellMoves locs wx =
  case wellTop locs wx of
    Nothing -> []
    Just w -> map (w,) $ lefts ++ rights
      where
        rights = takeEmpties $ dropWhile (`leftOf` w) $ hall
        lefts = takeEmpties $ dropWhile (`rightOf` w) $ reverse hall
        takeEmpties = takeWhile (isNothing . (locs Map.!))

leftOf :: Loc -> Loc -> Bool
leftOf (V2 x0 _) (V2 x1 _) = x0 < x1

rightOf :: Loc -> Loc -> Bool
rightOf (V2 x0 _) (V2 x1 _) = x0 > x1
