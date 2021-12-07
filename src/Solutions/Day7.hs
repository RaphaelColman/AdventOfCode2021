module Solutions.Day7 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           GHC.Num             (Integer)
import           Text.Trifecta       (Parser, commaSep, integer)

aoc7 :: IO ()
aoc7 = do
  printSolutions 7 $ MkAoCSolution parseInput part1
  printSolutions 7 $ MkAoCSolution parseInput part2

parseInput :: Parser [Integer]
parseInput = commaSep integer

part1 :: [Integer] -> Integer
part1 = bestPosition (\a b -> abs (a - b))

part2 :: [Integer] -> Integer
part2 = bestPosition calculateFuel

bestPosition :: (Integer -> Integer -> Integer) -> [Integer] -> Integer
bestPosition f xs = minimum $ map totalFuel [1 .. maximum xs]
  where
    totalFuel x = sum $ map (f x) xs

calculateFuel :: Integer -> Integer -> Integer
calculateFuel a b = triangleX $ abs $ a - b
  where
    triangleX x = x * (x + 1) `div` 2
