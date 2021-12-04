module Solutions.Day24
  ( aoc24
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc24 :: IO ()
aoc24 = do
  printSolutions 24 $ MkAoCSolution parseInput part1
  printSolutions 24 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
