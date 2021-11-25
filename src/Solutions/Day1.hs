module Solutions.Day1
  ( aoc1
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Data.Foldable       (find)
import           Data.List           (tails)
import           Text.Trifecta       (Parser, TokenParsing (token), integer,
                                      some)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1 part2

type Expenses = [Integer]

parseInput :: Parser Expenses
parseInput = do
  some $ token integer

part1 :: Expenses -> Maybe Integer
part1 expenses = do
  (a, b) <- find (\(x, y) -> x + y == 2020) $ pairs expenses
  pure (a * b)

part2 :: Expenses -> Maybe Integer
part2 expenses = do
  (a, b, c) <- find (\(x, y, z) -> x + y + z == 2020) $ triplets expenses
  pure (a * b * c)

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

triplets :: [a] -> [(a, a, a)]
triplets l = [(x, y, z) | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs]
