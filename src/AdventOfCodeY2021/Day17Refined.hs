-- | See the 'Day17' module for my initial solution where I tried to solve
-- the puzzle as fast as possible. This module attempts a more refined and
-- elegant solution now that the time pressure is off.
module AdventOfCodeY2021.Day17Refined where

import Data.List
import Data.Maybe
import Linear

test, run :: ((Int, Int), (Int, Int))
test = ((20, 30), (-10, -5))
run = ((230, 283), (-107, -57))

-- The velocity v_n = (vx_n, vy_n) and position p_n = (px_n, py_n) are given by
-- discrete difference equations, in analogy to conventional physics where
-- position and momentum are given by continuous differential equations. Instead
-- of iterating the equations to compute each trajectory step-by-step in time
-- linear with the number of steps, we can *solve* the difference equations to
-- get a constant-time formula for the state at step n.
--
-- We know recurrence relations for vy and py, shown below and expanded once.
--
--   vy_n = vy_{n-1} - 1        = vy_{n-2} - 2
--   py_n = py_{n-1} + vy_{n-1} = (py_{n-2} + vy_{n-2}) + (vy_{n-2} - 1)
--                              = py_{n-2} + 2*vy_{n-2} - 1
--
-- Expand this a few times more and you'll start seeing a patter (exercise for
-- the reader). From this we can guess the solution (note the use of the fact
-- that sum([1..n]) = n*(n+1)/2):
--
--   vy_n = vy_0 - n
--   py_n = py_0 + n*vy_0 - n*(n-1)/2
--
-- and prove that it is true by induction on n:
--
--   Base case n = 0:
--     vy_0 = vy_0 - 0 = vy_0
--     py_0 = py_0 + 0*vy_0 - 0*(-1/2) = py_0
--
--   Inductive case, prove for n + 1 assuming it is true for n:
--     vy_{n+1} = vy_n - 1 = (vy_0 - n) - 1 = vy_0 - (n + 1)
--     py_{n+1} = py_n + vy_n = py_0 + n*vy_0 - n*(n-1)/2 + vy_0 - n
--              = py_0 + (n+1)*vy_0 - (n*(n-1)/2 + n)
--              = py_0 + (n+1)*vy_0 - (n+1)*n/2

-- | 'yStep (vy, py) n' gives the y position and velocity at step n given
-- starting velocity vy and starting position py.
--
-- >>> take 10 $ iterate' (\(vy, py) -> (vy - 1, py + vy)) (3, 0)
-- [(3,0),(2,3),(1,5),(0,6),(-1,6),(-2,5),(-3,3),(-4,0),(-5,-4),(-6,-9)]
--
-- >>> take 10 $ map (yStep (3, 0)) [0..]
-- [(3,0),(2,3),(1,5),(0,6),(-1,6),(-2,5),(-3,3),(-4,0),(-5,-4),(-6,-9)]
yStep :: (Int, Int) -> Int -> (Int, Int)
yStep (vy_0, py_0) n = (vy_n, py_n)
  where
    vy_n = vy_0 - n
    py_n = py_0 + n*vy_0 - n*(n-1) `div` 2

-- Assuming the target area y coordinates are both negative and neglecting the x
-- coordinate, we can analytically determine the maximum height needed for Part
-- 1 of the Advent of Code Day 17 2021 puzzle. First, it is straightforward to
-- see that the initial y velocity vy_0 has to be positive, and higher initial
-- velocity corresponds to higher max height. So we want to find the highest
-- initial velocity that still hits the target.
--
-- Let vy_0 = n. Then after n steps, vy_n = 0 and py_n has reached its maximum
-- value of sum([0..n]) = n*(n+1)/2. py_{n+1} is the same as py_n since vy_n =
-- 0. By symmetry, n steps later py must be 0 again: py_{2n+1} = 0, vy_{2n+1} =
-- -n-1. The fact that py always gets back to 0 is very important: we can now
-- just skip to this step and know that the next py will be -n-1. Now we can see
-- that the maximum n is the one where we just barely stay in the target on step
-- 2n+2: i.e., we want -n-1 = minY, which is equivalent to n = -minY-1.

-- | Solves part 1 given just the minimum y target coordinate, assuming the
-- target window is sufficiently wide that the x-coordinate can be neglected,
-- and that the target window is fully in the lower half-plane.
-- Example input had minimum y target coordinate of -10:
-- >>> part1 (-10)
-- 45
--
-- My puzzle input had minimum y target coordinate of -107:
-- >>> part1 (-107)
-- 5671
part1 :: Int -> Int
part1 minY = vy_0*(vy_0+1) `div` 2
  where
    vy_0 = -minY-1

-- | Solves from the full input.
-- >>> part1' test
-- 45
-- >>> part1' run
-- 5671
part1' :: ((Int, Int), (Int, Int)) -> Int
part1' = part1 . fst . snd

-- Note that this solution really does rely on the target window being fully in
-- the lower half-plane. If any part of the target window covers y = 0, then
-- the maximum height is unbounded, since ALL trajectories will eventually land
-- back at py = 0 at step 2*vy_0+1.

-- For Part 2 of the puzzle, first figure out what are valid initial y
-- velocities, and on which steps they hit the target window. Part 1 actually
-- gives us the maximum y velocity based on the minimum y coordinate. By
-- symmetry, the minimum y velocity is just the minimum y target coordinate.

maxYVel :: Int -> Int
maxYVel minY = -minY-1

minYVel :: Int -> Int
minYVel minY = minY

-- Given the vy_0 and assuming py_0 = 0, how many steps until it gets less than
-- maxY and minY?
--
--   m > py_n = n*vy_0 - n*(n-1)/2 = -1/2*n^2 + (vy_0 + 1/2)*n
--   0 < 1/2*n^2 - (vy_0 + 1/2)*n + m
--   ==> n > b + sqrt(b^2 - 2*m)  where b = vy_0 + 1/2

-- >>> map (\vy_0 -> (vy_0, stepRangeY (-10, -5) vy_0)) [minYVel (-10)..maxYVel (-10)]
-- [(-10,[1]),(-9,[1]),(-8,[1]),(-7,[1]),(-6,[1]),(-5,[1]),(-4,[2]),(-3,[2]),(-2,[2,3]),(-1,[3,4]),(0,[4,5]),(1,[5,6]),(2,[7]),(3,[9]),(4,[10]),(5,[12]),(6,[14]),(7,[16]),(8,[18]),(9,[20])]
stepRangeY :: (Int, Int) -> Int -> (Int, Int)
stepRangeY (minY, maxY) vy_0 = (minN, maxN)
  where
    minN = ceiling $ b + sqrt(b^2 - 2*fromIntegral maxY)
    maxN = floor $ b + sqrt(b^2 - 2*fromIntegral minY)
    b = fromIntegral vy_0 + 1/2

-- Now we have to deal with the x-coordinate. We assume that the x-coordinates
-- of the target window are in the right half- plane (x > 0), which implies that
-- the initial velocity vx_0 > 0. The x-coord is always slowing down due to
-- drag, and will reach 0 after vx_0 steps. The difference equations are again a
-- recurrence relation that can be solved:
--
--   vx_n = vx_{n-1} - sgn(vx_{n-1})
--   px_n = px_{n-1} + vx_{n-1}
--
-- Solution is:
--
--   vx_n | n <  |vx_0| = vx_0 - sgn(vx_0)*n
--        | n >= |vx_0| = 0
--   px_n | n <  |vx_0| = px_0 + n*vx_0 - sgn(vx_0) * n*(n-1)/2
--        | n >= |vx_0| = px_0 + |vx_0|*vx_0 - sgn(vx_0) * |vx_0|*(|vx_0|-1)/2
--                      = px_0 + sgn(vx_0) * |vx_0|*(|vx_0|+1)/2

-- | 'xStep (vx, px) n' gives the x position and velocity at step n given
-- starting velocity vx and starting position px.
--
-- >>> take 8 $ iterate' (\(vx, px) -> (vx - signum vx, px + vx)) (5, 10)
-- [(5,10),(4,15),(3,19),(2,22),(1,24),(0,25),(0,25),(0,25)]
--
-- >>> take 8 $ map (xStep (5, 10)) [0..]
-- [(5,10),(4,15),(3,19),(2,22),(1,24),(0,25),(0,25),(0,25)]
xStep :: (Int, Int) -> Int -> (Int, Int)
xStep (vx_0, px_0) n = (vx_n, px_n)
  where
    vx_n | n < abs vx_0 = vx_0 - signum vx_0 * n
         | otherwise    = 0
    px_n | n < abs vx_0 = px_0 + n*vx_0 - signum vx_0 * n*(n-1) `div` 2
         | otherwise    = px_0 + signum vx_0 * abs vx_0*(abs vx_0+1) `div` 2

-- Given vx_0 > 0 and assuming px_0 = 0, we want to know at which steps minX <=
-- px_n and maxX >= px_n (also assuming maxX > minX > 0).
--
--   minX <= px_n = n*vx_0 - 1/2*n^2 + 1/2*n
--   ==> 0 >= n^2 - (2*vx_0 + 1)*n + 2*minX
--   ==> n = ( b +- sqrt(b^2 - 8*minX) ) / 2 where b = 2*vx_0 + 1
--   ==> n >= (b - sqrt(b^2 - 8*minX))/2 && n <= (b + sqrt(b^2 - 8*minX))/2
--
-- Since we know that n < vx_0 for the above expression, the second inequality
-- is free, and we can simply say minX <= px_n whenever
-- n >= (b-sqrt(b^2-8*minX))/2. From the discriminant, we know no n will work
-- when (2*vx_0 + 1)^2 < 8*minX <==> vx_0*(vx_0+1)/2 + 1/8 < minX. Note that
-- max{px_n} = vx_0*(vx_0+1)/2. Since we are only dealing with integers, the 1/8
-- doesn't make a difference and we can say no n will work whenever
-- max{px_n} < minX. This aligns with intuition, since if the maximal x position
-- never reaches the target minimum, of course no n will work!
--
-- What we want to find is, given minX, what is the minimum vx_0. And I think
-- later we will also need to know for each vx_0, how many steps until it
-- gets past the target minimum. Need (2*vx_0+1)^2 >= 8*minX <==
-- 2*vx_0+1 >= 2*sqrt(2*minX) <==> vx_0 >= sqrt(2*minX) - 1/2

-- >>> map getMinVx [1..20]
-- [1,2,2,3,3,3,4,4,4,4,5,5,5,5,5,6,6,6,6,6]
getMinVx :: Int -> Int
getMinVx minX = ceiling $ sqrt(2*fromIntegral minX) - 1/2

-- >>> map maxPx [1..20]
-- [1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210]
maxPx :: Int -> Int
maxPx vx_0 = vx_0*(vx_0+1) `div` 2

-- Similarly, we now want to find the maximum vx_0 for a given maxX. Solve the
-- inequality:
--
--   maxX >= px_n = min(vx_0*(vx_0+1)/2, n*vx_0 - 1/2*n^2 + 1/2*n)
--   ==> 0 <= n^2 - (2*vx_0 + 1)*n + 2*maxX
--   ==> n <= (b - sqrt(b^2 - 8*maxX))/2 || n >= (b + sqrt(b^2 - 8*minX))/2
--   ==> n <= (b - sqrt(b^2 - 8*maxX))/2  because second half is false
--
-- Given n solve for vx_0:
--
--   For n <= vx_0:
--     minX <= px_n = n*vx_0 - 1/2*n^2 + 1/2*n <= maxX
--     ==> minX/n + (n-1)/2 <= vx_0 <= maxX/n + (n-1)/2
--   For n > vx_0:
--     minX <= 1/2*vx_0^2 + 1/2*vx_0 <= maxX
--     ==> vx_0 = -1/2 +/- sqrt(1/4 + 2*mX)
--     ==> sqrt(1/4 + 2*minX) - 1/2 <= vx_0 <= sqrt(1/4 + 2*maxX) - 1/2

-- | Given the minX, maxX, and step n, compute the range of initial x velocities
-- that will land in the target on step n.
-- >>> map (\n -> (n, validVxs (20, 30) n)) [1..10]
-- [(1,(20,30)),(2,(11,15)),(3,(8,11)),(4,(7,9)),(5,(6,8)),(6,(6,7)),(7,(6,7)),(8,(6,7)),(9,(6,7)),(10,(6,7))]
validVxs :: (Int, Int) -> Int -> (Int, Int)
validVxs (minX, maxX) n =
  let vx_0min = if n > vx_0minTrans
                then vx_0minAsymp
                else vx_0minTrans
      vx_0max = if n > vx_0maxTrans
                then vx_0maxAsymp
                else vx_0maxTrans
  in (vx_0min, vx_0max)
  where
    vx_0minTrans = ceiling $ minX'/n' + (n'-1)/2
    vx_0maxTrans = floor $ maxX'/n' + (n'-1)/2
    vx_0minAsymp = ceiling $ sqrt (1/4 + 2*minX') - 1/2
    vx_0maxAsymp = floor $ sqrt (1/4 + 2*maxX') - 1/2
    minX' = fromIntegral minX
    maxX' = fromIntegral maxX
    n' = fromIntegral n

-- Now we put it all together for Part 2. There is still some "brute force"
-- involved, but the bounds are much leaner, and there is no iteration of the
-- trajectory. This makes it run quite a bit faster.

allValids :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
allValids (minX, maxX) (minY, maxY) =
  let vy_0s = [minYVel minY..maxYVel minY]
      mkStepsY vy_0 = let (a, b) = stepRangeY (minY, maxY) vy_0 in [a..b]
      stepsY = [(vy_0, step) | vy_0 <- vy_0s, step <- mkStepsY vy_0]
      mkVxs step = let (a, b) = validVxs (minX, maxX) step in [a..b]
  in  nub [(vx_0, vy_0) | (vy_0, step) <- stepsY, vx_0 <- mkVxs step]

part2 :: (Int, Int) -> (Int, Int) -> Int
part2 xr yr = length $ allValids xr yr

-- >>> part2' test
-- 112
-- >>> part2' run
-- 4556
part2' :: ((Int, Int), (Int, Int)) -> Int
part2' = uncurry part2
