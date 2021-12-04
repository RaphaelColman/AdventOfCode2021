module Solutions.Day18
  ( aoc18
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc18 :: IO ()
aoc18 = do
  printSolutions 18 $ MkAoCSolution parseInput part1
  printSolutions 18 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
