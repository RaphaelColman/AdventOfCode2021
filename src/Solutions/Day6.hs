module Solutions.Day6
  ( aoc6
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (freqs)
import           Data.List           (group)
import qualified Data.Map            as M
import qualified Data.Sequence       as S
import           Text.Trifecta       (Parser, commaSep, integer)
import Control.Monad.Trans.State.Lazy (runState, modify)

aoc6 :: IO ()
aoc6 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

type Age = Integer

type FishColony = M.Map Age Integer

parseInput :: Parser [Integer]
parseInput = commaSep integer

part1 :: [Integer] -> Integer
part1 = runFishColony 80 . freqs

part2 :: [Integer] -> Integer
part2 = runFishColony 256 . freqs

modularDecrement :: Integer -> Integer
modularDecrement i =
  let aged = i - 1
   in if aged < 0
        then 6
        else aged

stepFishColony :: FishColony -> FishColony
stepFishColony fc = snd . flip runState fc $ do
      modify $ M.insert 9 numZeros
      modify $ M.mapKeysWith (+) modularDecrement
      modify $ M.filter (/=0)
  where
    numZeros = M.findWithDefault 0 0 fc

runFishColony :: Int -> FishColony -> Integer
runFishColony times fc = M.foldr (+) 0 $ iterate stepFishColony fc !! times
