module Solutions.Day23
  ( aoc23
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc23 :: IO ()
aoc23 = do
  printSolutions 23 $ MkAoCSolution parseInput part1
  printSolutions 23 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
