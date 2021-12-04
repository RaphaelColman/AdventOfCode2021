module Solutions.Day13
  ( aoc13
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc13 :: IO ()
aoc13 = do
  printSolutions 13 $ MkAoCSolution parseInput part1
  printSolutions 13 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
