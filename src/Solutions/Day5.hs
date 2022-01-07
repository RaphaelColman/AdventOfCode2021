module Solutions.Day5 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (freqs, flexibleRange)
import           Common.Predicates   (anyPred)
import           Control.Lens        ((^.))
import           Data.List           (groupBy)
import qualified Data.Map            as M
import           Linear.V2           (R1 (_x), R2 (_y), V2 (..))
import           Text.Trifecta       (CharParsing (string), Parser, comma,
                                      commaSep, integer, newline, some,
                                      whiteSpace)

aoc5 :: IO ()
aoc5 = do
  printSolutions 5 $ MkAoCSolution parseInput part1
  printSolutions 5 $ MkAoCSolution parseInput part2

type Point = V2 Integer

type Line = (Point, Point)

parseInput :: Parser [Line]
parseInput = some parseLine

parseLine :: Parser Line
parseLine = do
  [x1, y1] <- commaSep integer
  string "->"
  whiteSpace
  [x2, y2] <- commaSep integer
  pure (V2 x1 y1, V2 x2 y2)

part1 :: [Line] -> Int
part1 lines =
  let orthogonals = filter (anyPred [isHorizontal, isVertical]) lines
   in M.size $ M.filter (>= 2) $ freqs $ concatMap pointsCovered orthogonals

part2 :: [Line] -> Int
part2 lines =
  let frqs = freqs $ concatMap pointsCovered lines
   in M.size $ M.filter (>= 2) frqs

pointsCovered :: Line -> [Point]
pointsCovered line
  | isHorizontal line = horizontalPointsCovered line
  | isVertical line = verticalPointsCovered line
  | otherwise = diagonalPointsCovered line

horizontalPointsCovered :: Line -> [Point]
horizontalPointsCovered (V2 x1 y1, V2 x2 y2) =
  map (`V2` y1) $ flexibleRange x1 x2

verticalPointsCovered :: Line -> [Point]
verticalPointsCovered (V2 x1 y1, V2 x2 y2) = map (V2 x1) $ flexibleRange y1 y2

diagonalPointsCovered :: Line -> [Point]
diagonalPointsCovered (V2 x1 y1, V2 x2 y2) =
  zipWith V2 (flexibleRange x1 x2) (flexibleRange y1 y2)

isHorizontal :: Line -> Bool
isHorizontal (v1, v2) = v1 ^. _y == v2 ^. _y

isVertical :: Line -> Bool
isVertical (v1, v2) = v1 ^. _x == v2 ^. _x