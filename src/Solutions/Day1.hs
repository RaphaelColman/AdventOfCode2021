module Solutions.Day1
  ( aoc1
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.List           (tails)
import           Text.Trifecta       (Parser, TokenParsing (token), integer,
                                      some)

aoc1 :: IO ()
aoc1 = do
  printSolutions 1 $ MkAoCSolution parseInput part1
  printSolutions 1 $ MkAoCSolution parseInput part2

type Depths = [Integer]

parseInput :: Parser Depths
parseInput = do
  some $ token integer

part1 :: Depths -> Int
part1 = sonarSweep

part2 :: Depths -> Int
part2 = sonarSweep . map (\(x, y, z) -> x + y + z) . window3

sonarSweep :: Depths -> Int
sonarSweep = length . filter id . map (\(x, y) -> y > x) . window2

window2 :: [a] -> [(a, a)]
window2 l@(_:xs) = zip l xs
window2 _        = []

window3 :: [a] -> [(a, a, a)]
window3 l@(_:y:xs) = zip3 l (y : xs) xs
window3 _          = []

windowN :: Int -> [a] -> [[a]]
windowN n xs = filter ((== n) . length) $ map (take n) $ tails xs
