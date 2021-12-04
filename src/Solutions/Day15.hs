module Solutions.Day15
  ( aoc15
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc15 :: IO ()
aoc15 = do
  printSolutions 15 $ MkAoCSolution parseInput part1
  printSolutions 15 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
