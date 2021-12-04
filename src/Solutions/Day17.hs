module Solutions.Day17
  ( aoc17
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Text.Trifecta       (Parser)

aoc17 :: IO ()
aoc17 = do
  printSolutions 17 $ MkAoCSolution parseInput part1
  printSolutions 17 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = undefined

part1 :: String -> String
part1 = undefined

part2 :: String -> String
part2 = undefined
