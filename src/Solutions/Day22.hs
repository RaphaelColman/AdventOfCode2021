module Solutions.Day22
  ( aoc22
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 $ MkAoCSolution parseInput part1
  printSolutions 22 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
