module Solutions.Day25
  ( aoc25
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc25 :: IO ()
aoc25 = do
  printSolutions 25 $ MkAoCSolution parseInput part1
  printSolutions 25 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
