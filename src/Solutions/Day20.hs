module Solutions.Day20
  ( aoc20
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc20 :: IO ()
aoc20 = do
  printSolutions 20 $ MkAoCSolution parseInput part1
  printSolutions 20 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
