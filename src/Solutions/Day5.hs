module Solutions.Day5 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (freqs)
import           Control.Lens        ((^.))
import qualified Data.Map            as M
import           Debug.Trace
import           Linear.V2
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
  let horizontals = filter isHorizontal lines
      hPoints = concatMap horizontalPointsCovered horizontals
      verticals = filter isVertical lines
      vPoints = concatMap verticalPointsCovered verticals
      frqs = freqs $ hPoints ++ vPoints
      overlaps = M.filter (>= 2) frqs
   in M.size overlaps

part2 :: [Line] -> Int
part2 lines =
  let horizontals = filter isHorizontal lines
      hPoints = concatMap horizontalPointsCovered horizontals
      verticals = filter isVertical lines
      vPoints = concatMap verticalPointsCovered verticals
      rest = filter (\l -> not (isHorizontal l) && not (isVertical l)) lines
      dPoints = concatMap diagonalPointsCovered rest
      frqs = freqs $ hPoints ++ vPoints ++ dPoints
      overlaps = M.filter (>= 2) frqs
   in M.size overlaps

horizontalPointsCovered :: Line -> [Point]
horizontalPointsCovered (V2 x1 y1, V2 x2 y2) =
  map (`V2` y1) [(min x1 x2) .. (max x1 x2)]

verticalPointsCovered :: Line -> [Point]
verticalPointsCovered (V2 x1 y1, V2 x2 y2) =
  map (V2 x1) [(min y1 y2) .. (max y1 y2)]

diagonalPointsCovered :: Line -> [Point]
diagonalPointsCovered (V2 x1 y1, V2 x2 y2) =
  zipWith V2 (flexibleRange x1 x2) (flexibleRange y1 y2)

isHorizontal :: Line -> Bool
isHorizontal (v1, v2) = v1 ^. _y == v2 ^. _y

isVertical :: Line -> Bool
isVertical (v1, v2) = v1 ^. _x == v2 ^. _x

flexibleRange :: Integer -> Integer -> [Integer]
flexibleRange a b
  | b >= a = [a .. b]
  | otherwise = [a,(a - 1) .. b]
