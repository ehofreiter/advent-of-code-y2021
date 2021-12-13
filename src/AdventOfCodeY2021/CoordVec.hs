{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module AdventOfCodeY2021.CoordVec
  ( CoordVec
  , fromLists
  , toLists
  , toList
  , rowSize
  , colSize
  , rowCount
  , colCount
  , (!)
  , (!?)
  , adjs4
  , adjCoords4
  , adjPairs4
  , adjs8
  , adjCoords8
  , adjPairs8
  ) where

import           Data.List.Split
import           Data.Functor.WithIndex
import           Data.Maybe
import qualified Data.Vector as Vec

type Coord = (Int, Int)
data CoordVec a = CoordVec
  { rowSize :: Int
  , colSize :: Int
  , coordVec :: Vec.Vector a
  }
  deriving (Eq, Show)

instance Functor CoordVec where
  fmap f = overVec (fmap f)

instance FunctorWithIndex (Int, Int) CoordVec where
  imap f cv = overVec (Vec.imap f') cv
    where
      f' i = f (unflattenCoord cv i)

flattenCoord :: CoordVec a -> Coord -> Int
flattenCoord cv (x, y) = x + y * colCount cv

unflattenCoord :: CoordVec a -> Int -> Coord
unflattenCoord cv i = (x, y)
  where
    y = i `div` colCount cv
    x = i `mod` colCount cv

-- | Apply a map over the inner Vector. Promise not to change the size!
overVec :: (Vec.Vector a -> Vec.Vector b) -> CoordVec a -> CoordVec b
overVec f cv = cv { coordVec = f (coordVec cv) }

empty :: CoordVec a
empty = CoordVec
  { rowSize = 0
  , colSize = 0
  , coordVec = Vec.empty
  }

singleton :: a -> CoordVec a
singleton x = CoordVec
  { rowSize = 1
  , colSize = 1
  , coordVec = Vec.singleton x
  }

replicate :: Int -> Int -> a -> CoordVec a
replicate numCols numRows x = CoordVec
  { rowSize = numCols
  , colSize = numRows
  , coordVec = Vec.replicate (numCols * numRows) x
  }

fromLists :: [[a]] -> CoordVec a
fromLists [] = CoordVec
  { rowSize = 0
  , colSize = 0
  , coordVec = Vec.empty
  }
fromLists (r:rs) = CoordVec
  { rowSize = length r
  , colSize = length (r:rs)
  , coordVec = Vec.fromList $ concat (r:rs)
  }

toLists :: CoordVec a -> [[a]]
toLists cv = chunksOf (rowSize cv) $ toList cv

toList :: CoordVec a -> [a]
toList = Vec.toList . coordVec

rowCount :: CoordVec a -> Int
rowCount = colSize

colCount :: CoordVec a -> Int
colCount = rowSize

(!) :: CoordVec a -> (Int, Int) -> a
(!) cv (x, y) | x < 0 =
  error $ "Negative col " ++ show x
(!) cv (x, y) | x >= rowSize cv =
  error $ "Col " ++ show x ++ " not less than colCount " ++ show (colCount cv)
(!) cv (x, y) | y < 0 =
  error $ "Negative row " ++ show y
(!) cv (x, y) | y >= colSize cv =
  error $ "Row " ++ show y ++ " not less than rowCount " ++ show (rowCount cv)
(!) cv (x, y) = coordVec cv Vec.! (x + y * rowSize cv)

(!?) :: CoordVec a -> (Int, Int) -> Maybe a
(!?) cv (x, y) | x < 0
              || x >= rowSize cv
              || y < 0
              || y >= colSize cv = Nothing
               | otherwise       = coordVec cv Vec.!? (x + y * rowSize cv)

leftCol :: CoordVec a -> Int -> Maybe Int
leftCol cv x | x > 0     = Just (x - 1)
             | otherwise = Nothing

rightCol :: CoordVec a -> Int -> Maybe Int
rightCol cv x | x < colCount cv - 1 = Just (x + 1)
              | otherwise           = Nothing

aboveRow :: CoordVec a -> Int -> Maybe Int
aboveRow cv y | y > 0     = Just (y - 1)
              | otherwise = Nothing

belowRow :: CoordVec a -> Int -> Maybe Int
belowRow cv y | y < rowCount cv - 1 = Just (y + 1)
              | otherwise           = Nothing

adjCols :: CoordVec a -> Int -> [Int]
adjCols cv x = catMaybes [leftCol cv x, rightCol cv x]

adjRows :: CoordVec a -> Int -> [Int]
adjRows cv y = catMaybes [aboveRow cv y, belowRow cv y]

adjColCoords :: CoordVec a -> Coord -> [Coord]
adjColCoords cv (x, y) = fmap (, y) $ adjCols cv x

adjRowCoords :: CoordVec a -> Coord -> [Coord]
adjRowCoords cv (x, y) = fmap (x ,) $ adjRows cv y

adjDiagCoords :: CoordVec a -> Coord -> [Coord]
adjDiagCoords cv (x, y) = (,) <$> adjCols cv x <*> adjRows cv y

adjCoords4 :: CoordVec a -> Coord -> [Coord]
adjCoords4 cv c = adjColCoords cv c <> adjRowCoords cv c

adjCoords8 :: CoordVec a -> Coord -> [Coord]
adjCoords8 cv c = adjCoords4 cv c <> adjDiagCoords cv c

adjs4 :: CoordVec a -> Coord -> [a]
adjs4 cv c = fmap (cv !) $ adjCoords4 cv c

adjs8 :: CoordVec a -> Coord -> [a]
adjs8 cv c = fmap (cv !) $ adjCoords8 cv c

-- >>> adjPairs4 (fromLists ["ab","cd"]) (0,0)
-- [((1,0),'b'),((0,1),'c')]
-- >>> adjPairs4 (fromLists ["abc","def","ghi"]) (1,1)
-- [((0,1),'d'),((2,1),'f'),((1,0),'b'),((1,2),'h')]
-- >>> adjPairs4 (fromLists ["abc","def","ghi"]) (3,1)
-- [((2,1),'f'),((3,0),*** Exception: Col 3 not less than colCount 3
-- >>> adjPairs4 (fromLists ["abc","def","ghi"]) (1,3)
-- [((0,3),*** Exception: Row 3 not less than rowCount 3
adjPairs4 :: CoordVec a -> Coord -> [(Coord, a)]
adjPairs4 cv c = fmap f $ adjCoords4 cv c
  where
    f c' = (c', cv ! c')

-- >>> adjPairs8 (fromLists ["ab","cd"]) (0,0)
-- [((1,0),'b'),((0,1),'c'),((1,1),'d')]
-- >>> adjPairs8 (fromLists ["abc","def","ghi"]) (1,1)
-- [((0,1),'d'),((2,1),'f'),((1,0),'b'),((1,2),'h'),((0,0),'a'),((0,2),'g'),((2,0),'c'),((2,2),'i')]
adjPairs8 :: CoordVec a -> Coord -> [(Coord, a)]
adjPairs8 cv c = fmap f $ adjCoords8 cv c
  where
    f c' = (c', cv ! c')
