module AdventOfCodeY2021.Day16 where

import Control.Lens
import Data.Bits
import Data.Char
import Data.Foldable
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Traversable
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq((:|>), (:<|)), (><))
import qualified Data.Set as Set
import Numeric
import qualified Text.Parsec as P

import AdventOfCodeY2021.Common

import AdventOfCodeY2021.CoordVec as CV

run = runWith "data/day16/input.txt"
test = runWith "data/day16/test.txt"

runWith :: FilePath -> IO ()
runWith filePath = do
  strs <- readInputs filePath id
  let bits = concatMap hexToBits $ head strs
      result = P.parse parsePackets "runWith" (bitsToStr bits)
  print (sumVersions . head <$> result)
  print (evaluate . head <$> result)

evaluate :: Packet -> Int
evaluate ((v, t), b) =
  case b of
    Literal n -> n
    Operator ps ->
      let xs = map evaluate ps
      in  case t of
        0 -> sum xs
        1 -> product xs
        2 -> minimum xs
        3 -> maximum xs
        5 -> let [a,b] = xs in if a > b then 1 else 0
        6 -> let [a,b] = xs in if a < b then 1 else 0
        7 -> let [a,b] = xs in if a == b then 1 else 0

sumVersions :: Packet -> Int
sumVersions ((v, t), b) =
  case b of
    Literal _ -> v
    Operator ps -> v + sum (map sumVersions ps)

runParse str = do
  let bits = bitsToStr $ concatMap hexToBits str
  print bits
  pure $ parseBits bits

parseIt str =
  let bits = concatMap hexToBits str
  in  P.parse parsePackets "parseIt" (bitsToStr bits)

parseBits str =
  P.parse parsePackets "parseBits" str

type Packet = ((Int, Int), Body)
data Body
  = Literal Int
  | Operator [Packet]
  deriving (Eq, Show)

type Par a = P.Parsec String () a

parsePackets :: Par [Packet]
parsePackets = P.manyTill parsePacket end
  where
    end = P.try $ P.many (P.char '0') *> P.eof
    -- ^ Without "try" here, end will consume all leading zeros and cause
    -- hours of absolute pain. I have learned to truly despise Parsec. On
    -- the other hand, I couldn't have solved this so easily without Parsec.
    -- So I'm conflicted.

parsePacket :: Par Packet
parsePacket = do
  version <- bitsToInt <$> P.count 3 bitP P.<?> "version"
  typeID <- bitsToInt <$> P.count 3 bitP P.<?> "typeID"
  body <- parseBody typeID
  pure ((version, typeID), body)

parseBody :: Int -> Par Body
parseBody typeID =
  if typeID == 4
  then Literal <$> parseLiteral []
  else Operator <$> parseOperator

parseOperator :: Par [Packet]
parseOperator = do
  ltid <- bitP
  if ltid
  then do
    packetCount <- bitsToInt <$> P.count 11 bitP
    P.count packetCount parsePacket
  else do
    bitCount <- bitsToInt <$> P.count 15 bitP
    bodyStr <- bitsToStr <$> P.count bitCount bitP
    case P.parse parsePackets ("op: "++show bitCount++ " "++bodyStr)  bodyStr of
      Left err -> fail (show err)
      Right packets -> pure packets

parseLiteral :: [Bool] -> Par Int
parseLiteral bits = do
  chunk <- P.count 5 bitP
  if head chunk
  then parseLiteral (bits ++ tail chunk)
  else pure $ bitsToInt (bits ++ tail chunk)

bitP :: Par Bool
bitP = charToBit <$> P.oneOf "01"

getVersion :: [Bool] -> Int
getVersion = bitsToInt . take 3

getTypeID :: [Bool] -> Int
getTypeID = bitsToInt . take 3 . drop 3

strToBits :: String -> [Bool]
strToBits = map charToBit

charToBit :: Char -> Bool
charToBit '0' = False
charToBit '1' = True

bitsToInt :: [Bool] -> Int
bitsToInt = sum . zipWith f [0..] . reverse
  where
    f i True = 2^i
    f i False = 0

bitsToStr :: [Bool] -> String
bitsToStr = map f
  where
    f True = '1'
    f False = '0'

hexToBits :: Char -> [Bool]
hexToBits c = map (n `testBit`) [3,2,1,0]
  where
    n = fst $ head $ Numeric.readHex [c] :: Int
