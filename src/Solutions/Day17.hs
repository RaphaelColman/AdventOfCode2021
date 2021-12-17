module Solutions.Day17
  ( aoc17
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Geometry     (Point)
import           Common.MathUtils    (triangleX)
import           Common.Predicates   (allPred)
import           Control.Lens        ((^.))
import           Control.Monad.Loops (unfoldrM)
import           Data.List           (unfoldr)
import           Data.Maybe          (mapMaybe)
import           Linear.V2           (R2 (_y), V2 (..))
import           Text.Trifecta       (CharParsing (string), Parser, integer)

aoc17 :: IO ()
aoc17 = do
  printSolutions 17 $ MkAoCSolution parseInput part1
  printSolutions 17 $ MkAoCSolution parseInput part2

type TargetArea = (Point, Point)

parseInput :: Parser TargetArea
parseInput = do
  string "target area: x="
  xMin <- fromInteger <$> integer
  string ".."
  xMax <- fromInteger <$> integer
  string ", y="
  yMin <- fromInteger <$> integer
  string ".."
  yMax <- fromInteger <$> integer
  pure (V2 xMin yMin, V2 xMax yMax)

part1 :: TargetArea -> Int
part1 = maximum . map (^. _y) . concat . vectorRange

part2 :: TargetArea -> Int
part2 = length . vectorRange

vectorRange :: TargetArea -> [[V2 Int]]
vectorRange ta@(V2 xmin ymin, V2 xmax ymax) = mapMaybe (`fireProbe` ta) vRange
  where
    xRange = filter (\x -> triangleX x >= xmin) [1 .. xmax] --if the corresponding triangle number is not big enough, then our probe will never get far enough
    yRange = [ymin .. abs ymin]
    vRange = [V2 x y | x <- xRange, y <- yRange]

inTargetArea :: Point -> TargetArea -> Bool
inTargetArea (V2 x y) (V2 xmin ymin, V2 xmax ymax) = inX x && inY y
  where
    inX val = allPred [(>= xmin), (<= xmax)] val
    inY val = allPred [(>= ymin), (<= ymax)] val

passedTargetArea :: Point -> TargetArea -> Bool
passedTargetArea (V2 x y) (V2 xmin ymin, V2 xmax ymax) = passedX x || passedY y
  where
    passedX val = val > xmax
    passedY val = val < ymin

type Velocity = V2 Int

type ProbeState = (Point, Velocity)

fireProbe :: Velocity -> TargetArea -> Maybe [Point]
fireProbe vel targetArea = unfoldrM go (V2 0 0, vel)
  where
    go :: ProbeState -> Maybe (Maybe (Point, ProbeState))
    go (current, vel@(V2 xVel yVel))
      | passedTargetArea current targetArea = Nothing
      | inTargetArea current targetArea = pure Nothing
      | otherwise = pure $ Just (nextPoint, next)
      where
        next@(nextPoint, _) = (current + vel, stepVelocity vel)

stepVelocity :: V2 Int -> V2 Int
stepVelocity (V2 x y)
  | x == 0 = V2 x (y - 1)
  | x > 0 = V2 (x - 1) (y - 1)
  | otherwise = V2 (x + 1) (y - 1)
