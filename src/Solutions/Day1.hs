module Solutions.Day1
  ( aoc1
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.List           (tails)
import           Text.Trifecta       (Parser, TokenParsing (token), integer,
                                      some)
import Common.ListUtils (window3, window2)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

type Depths = [Integer]

parseInput :: Parser Depths
parseInput = do
  some $ token integer

part1 :: Depths -> Int
part1 = sonarSweep

part2 :: Depths -> Int
part2 = sonarSweep . map (\(x, y, z) -> x + y + z) . window3

sonarSweep :: Depths -> Int
sonarSweep = length . filter id . map (\(x, y) -> y > x) . window2
