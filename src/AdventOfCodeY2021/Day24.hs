{-# LANGUAGE TupleSections #-}
module AdventOfCodeY2021.Day24 where

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

runF = "data/day24/input.txt"

load :: FilePath -> IO [Instr]
load filePath = do
  readInputs filePath readInstr

-- fromList [(0, a0), (1, a1), (2, a2), ...]
-- means a0 + 26*a1 + 26^2*a2 + ...
-- Like a base 26 number
type ZNum a = Map.Map Int a

-- model number digit ID (most significant digit ID is 1, then 2, up to 14)
type DID = Int

-- One of the model number digits plus an integer
-- DPlus 1 5 means d_1 + 5, where d_1 is the first model number digit
-- Note that model number digits are variables that can be in [1..9] (not 0)
data DExpr = DLit Int | DPlus DID Int
  deriving (Eq, Ord, Show)

-- either a plain znum, or a choice between two.
-- ZNEq (i, DPlus j 5) za zb means this:
--   if d_i /= d_j + 5 then za else zb
data ZResult a
  = ZNum a
  | ZNEq (DID, DExpr) (ZResult a) (ZResult a)
  deriving (Eq, Ord, Show)

instance Functor ZResult where
  fmap f zr = case zr of
    ZNum x -> ZNum (f x)
    ZNEq p x y -> ZNEq p (fmap f x) (fmap f y)

type Dir = (Bool, (DID, DExpr)) -- true means left branch, or not-equal

-- NB: may not be fully generic, but worked for my input.
part1 :: String
part1 =
  let zrs = getZeroResults stepAll
      digitValues = concatMap solveEqLowest (head zrs)
  in  concatMap (show . snd) (sort digitValues)

-- Given the Dir, which represents an equation, solve it optimizing for the
-- highest value. Lower digit IDs (DIDs) indicate more significance, i.e.
-- the first digit with DID = 1 is the most significant digit.
solveEqHighest :: Dir -> [(DID, Int)]
solveEqHighest (False, (dIdx, DPlus dIdy a)) | dIdx > dIdy -- d_x = d_y + a, y < x
  = let (vy0, vx0) = head [(vy, vy + a) | vy <- reverse [1..9], vy+a > 0 && vy+a <= 9]
    in  [(dIdy, vy0), (dIdx, vx0)]

-- NB: may not be fully generic, but worked for my input.
part2 :: String
part2 =
  let zrs = getZeroResults stepAll
      digitValues = concatMap solveEqLowest (head zrs)
  in  concatMap (show . snd) (sort digitValues)

-- Given the Dir, which represents an equation, solve it optimizing for the
-- lowest value. Lower digit IDs (DIDs) indicate more significance, i.e.
-- the first digit with DID = 1 is the most significant digit.
solveEqLowest :: Dir -> [(DID, Int)]
solveEqLowest (False, (dIdx, DPlus dIdy a)) | dIdx > dIdy -- d_x = d_y + a, y < x
  = let (vy0, vx0) = head [(vy, vy + a) | vy <- [1..9], vy+a > 0 && vy+a <= 9]
    in  [(dIdy, vy0), (dIdx, vx0)]

-- search the tree to find cases where the final leaf gives a value in the z
-- register of zero
getZeroResults :: ZResult (ZNum DExpr) -> [[Dir]]
getZeroResults zr = case zr of
  ZNum zn -> if zn == Map.empty then [[]] else []
  ZNEq p x y -> map ((True, p) :) (getZeroResults x)
             ++ map ((False, p) :) (getZeroResults y)

-- generate the tree of z-register values based on the input variables
stepAll :: ZResult (ZNum DExpr)
stepAll =
  let vars = [(ai i, bi i, ci i, i+1) | i <- [0..13]]
  in  foldl' (\zr var -> stepZNum var zr) (ZNum Map.empty) vars

-- I analyzed the instructions by hand to come up with a formula for efficiently
-- simulating the program. Note that the instruction set is just 14 blocks of 18
-- instructions. each block starts with inp w (call it instr 1), then has 17
-- instructions which are mostly similar but differ in a few key places:
--   instr 5:  div x (D_i)
--   instr 6:  add x (A_i)
--   instr 16: add y (B_i)

-- Here is the formula that each block is computing. The w, x, y registers are
-- temporary and cleared each block. Only the z register accumulates state.
-- We model its state as a recurrence relation z_i = f(z_{i-1}), parameterized
-- by the variable instructions D_i, A_i, and B_i described above. The initial
-- state is z_0 = 0. The goal is to find values for the d_i such that z_14 = 0.
-- The d_i must be a digit 1 through 9, excluding 0.
--   z_i = (z_{i-1} `div` D_i) * (25*E_i + 1) + (d_i + B_i) * E_i
--   E_i = if d_i /= A_i + z_{i-1} `mod` 26 then 1 else 0
--   A_i = (hard coded values from 6th instruction (add x A_i))
--   B_i = (hard coded values from 16th instruction (add y B_i))
--   D_i = (either 1 or 26, from 5th instruction (div z D_i))
--   d_i = the ith input digit of the model number

-- perform one set of instructions starting with inp w.stepZNum :: (Int, Int, Int, Int) -> ZResult (ZNum DExpr) -> ZResult (ZNum DExpr)
stepZNum (a, b, c, dId) (ZNum zn) =
  if a > 9
  then ZNum z1
  else zneq
  where
    z0 | c == 1  = zn
       | c == 26 = rShift zn
    z1 = Map.insert 0 (DPlus dId b) $ lShift z0
    -- ^ this means: 26*z0 + d_dId + b
    neqRhs = case mod26 zn of
      DLit i -> DLit (i + a)
      DPlus d i -> DPlus d (i + a)
    neqTest = (dId, neqRhs)
    zneq = ZNEq neqTest (ZNum z1) (ZNum z0)
stepZNum vars (ZNEq p zl zr) =
  ZNEq p (stepZNum vars zl) (stepZNum vars zr)

mod26 :: ZNum DExpr -> DExpr
mod26 = fromMaybe (DLit 0) . Map.lookup 0

-- like dividing by 26 and truncating down
rShift :: ZNum a -> ZNum a
rShift = Map.mapKeys pred . Map.delete 0

-- like multiplying by 26
lShift :: ZNum a -> ZNum a
lShift = Map.mapKeys succ

ai :: Int -> Int
ai i = (!! i) [12, 11, 10, 10, -16, 14, 12, -4, 15, -7, -8, -4, -15, -8]

bi :: Int -> Int
bi i = (!! i) [6, 12, 5, 10, 7, 0, 4, 12, 14, 13, 10, 11, 9, 9]

ci :: Int -> Int
ci i = (!! i) [1, 1, 1, 1, 26, 1, 1, 26, 1, 26, 26, 26, 26, 26]

-- Following is a bunch of stuff for simulating the ALU that I didn't need
-- for the problem.

runP :: (Int -> Expr) -> State -> [Instr] -> [(Instr, State)]
runP evar s0 instrs =
  zip instrs $ tail $ scanl' (doInstr evar) initState instrs

runPD :: (Int -> [Int]) -> StateD -> [Instr] -> [(Instr, StateD)]
runPD evar sd0 instrs =
  zip instrs $ tail $ scanl' (doInstrD evar) initStateD instrs

everyDigit :: Int -> [Int]
everyDigit = const [1..9]

all9s :: Int -> Expr
all9s = const (ELit 9)

printInstrState :: (Instr, State) -> IO ()
printInstrState (instr, s) = do
  print instr
  printState s

printState :: State -> IO ()
printState (ni, rs) = do
  putStrLn $ "Next Digit: d" <> show ni
  printRegState rs

printRegState :: RegState -> IO ()
printRegState rs = do
  putStrLn $ "W = " <> show (getR W rs)
  putStrLn $ "X = " <> show (getR X rs)
  putStrLn $ "Y = " <> show (getR Y rs)
  putStrLn $ "Z = " <> show (getR Z rs)

initState :: State
initState = (1, Map.fromList $ zip [W,X,Y,Z] (repeat (ELit 0)))

initRegState :: RegState
initRegState = Map.fromList $ zip [W,X,Y,Z] (repeat (ELit 0))

type RegStateD = Map.Map RegState [[Int]]
type StateD = (Int, Map.Map RegState [[Int]])

type RegD = Map.Map Expr [[Int]]

initRegStateD = Map.singleton initRegState [[]]

initStateD :: StateD
initStateD = (1, initRegStateD)

doInstrD :: (Int -> [Int]) -> StateD -> Instr -> StateD
doInstrD evar (ni, rsd) instr = case instr of
  Inp r -> (ni+1, inpRD r (evar ni) rsd)
  Add r v -> (ni, setRD r (\rs -> eadd (getR r rs) (valE v rs)) rsd)
  Mul r v -> (ni, setRD r (\rs -> emul (getR r rs) (valE v rs)) rsd)
  Div r v -> (ni, setRD r (\rs -> ediv (getR r rs) (valE v rs)) rsd)
  Mod r v -> (ni, setRD r (\rs -> emod (getR r rs) (valE v rs)) rsd)
  Eql r v -> (ni, setRD r (\rs -> eeql (getR r rs) (valE v rs)) rsd)

setRD :: Reg -> (RegState -> Expr) -> RegStateD -> RegStateD
setRD r rsexpr = Map.mapKeys (\rs -> setR r (rsexpr rs) rs)

inpRD :: Reg -> [Int] -> RegStateD -> RegStateD
inpRD r inputs rsd = Map.fromListWith union $ concatMap f $ Map.toList rsd
  where
    f (rs, dss) = map (\input -> (setR r (ELit input) rs, map (input:) dss)) inputs

doInstr :: (Int -> Expr) -> State -> Instr -> State
doInstr evar (ni, rs) instr = case instr of
  Inp r -> (ni+1, setR r (evar ni) rs)
  Add r v -> (ni, setR r (eadd (getR r rs) (valE v rs)) rs)
  Mul r v -> (ni, setR r (emul (getR r rs) (valE v rs)) rs)
  Div r v -> (ni, setR r (ediv (getR r rs) (valE v rs)) rs)
  Mod r v -> (ni, setR r (emod (getR r rs) (valE v rs)) rs)
  Eql r v -> (ni, setR r (eeql (getR r rs) (valE v rs)) rs)

eadd :: Expr -> Expr -> Expr
eadd (ELit a) (ELit b) = ELit (a + b)
eadd (ELit 0) eb = eb
eadd ea (ELit 0) = ea
eadd ea eb = EAdd ea eb

emul :: Expr -> Expr -> Expr
emul (ELit a) (ELit b) = ELit (a * b)
emul (ELit 0) eb = ELit 0
emul ea (ELit 0) = ELit 0
emul (ELit 1) eb = eb
emul ea (ELit 1) = ea
emul ea eb = EMul ea eb

ediv :: Expr -> Expr -> Expr
ediv (ELit a) (ELit b) = ELit (a `div` b)
ediv (ELit 0) eb = ELit 0
ediv ea (ELit 0) = error "ediv by 0"
ediv ea (ELit 1) = ea
ediv ea eb | ea == eb = ELit 1
           | otherwise = EDiv ea eb

emod :: Expr -> Expr -> Expr
emod (ELit a) (ELit b) = ELit (a `mod` b)
emod (ELit 0) eb = ELit 0
emod ea (ELit 0) = error "emod by 0"
emod ea (ELit 1) = ELit 0
emod ea (ELit (-1)) = ELit 0
emod ea eb | ea == eb = ELit 0
           | otherwise = EMod ea eb

eeql :: Expr -> Expr -> Expr
eeql (ELit a) (ELit b) | a == b = ELit 1
                       | otherwise = ELit 0
eeql (ELit a) (EVar _) | a <= 0 || a > 9 = ELit 0
eeql ea eb | ea == eb = ELit 1
           | otherwise = EEql ea eb

valE :: Val -> RegState -> Expr
valE v rs = case v of
  Reg r -> getR r rs
  Lit l -> ELit l

getR :: Reg -> RegState -> Expr
getR r rs = rs Map.! r

setR :: Reg -> Expr -> RegState -> RegState
setR = Map.insert

type State = (Int, RegState) -- next input

type RegState = Map.Map Reg Expr

data Expr
  = EVar Int
  | ELit Int
  | EAdd Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  | EMod Expr Expr
  | EEql Expr Expr
  deriving (Eq, Ord, Show)

data Instr
  = Inp Reg
  | Add Reg Val
  | Mul Reg Val
  | Div Reg Val
  | Mod Reg Val
  | Eql Reg Val
  deriving (Eq, Ord, Show)

data Val = Reg Reg | Lit Int
  deriving (Eq, Ord, Show)

data Reg = W | X | Y | Z
  deriving (Eq, Ord, Show)

readInstr :: String -> Instr
readInstr s =
  case splitOn " " s of
    ["inp", a] -> Inp (readReg a)
    ["add", a, b] -> Add (readReg a) (readVal b)
    ["mul", a, b] -> Mul (readReg a) (readVal b)
    ["div", a, b] -> Div (readReg a) (readVal b)
    ["mod", a, b] -> Mod (readReg a) (readVal b)
    ["eql", a, b] -> Eql (readReg a) (readVal b)

readReg :: String -> Reg
readReg "w" = W
readReg "x" = X
readReg "y" = Y
readReg "z" = Z

readVal :: String -> Val
readVal s | s `elem` ["w","x","y","z"] = Reg $ readReg s
          | otherwise = Lit $ read s
