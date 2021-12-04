module Solutions.Day8
  ( aoc8
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc8 :: IO ()
aoc8 = do
  printSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
