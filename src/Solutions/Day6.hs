module Solutions.Day6
  ( aoc6
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.List           (group)
import qualified Data.Sequence       as S
import           Text.Trifecta       (Parser, commaSep, integer)

aoc6 :: IO ()
aoc6
 = do
  printSolutions 6 $ MkAoCSolution parseInput part1
  printSolutions 6 $ MkAoCSolution parseInput part2

type Fish = [FishGeneration]

data FishGeneration =
  MkFishGeneration
    { _number :: Integer
    , _age    :: Integer
    }
  deriving (Eq, Show)

parseInput :: Parser [Integer]
parseInput = commaSep integer

initFishGenerationList :: [Integer] -> [FishGeneration]
initFishGenerationList xs =
  map (\fs -> MkFishGeneration (toInteger (length fs)) (head fs)) $ group xs

part1 :: [Integer] -> Integer
part1 xs = runFish xs 80

part2 :: [Integer] -> Integer
part2 xs = runFish xs 256

runFish :: [Integer] -> Int-> Integer
runFish xs times =
  let generations = initFishGenerationList xs
      iterated = iterate stepFish generations !! times
   in sum $ map _number iterated

modularDecrement :: Integer -> Integer
modularDecrement i =
  let aged = i - 1
   in if aged < 0
        then 6
        else aged

stepGeneration :: FishGeneration -> FishGeneration
stepGeneration (MkFishGeneration number age) =
  let aged = age - 1
   in if aged < 0
        then MkFishGeneration number 6
        else MkFishGeneration number aged

stepFish :: [FishGeneration] -> [FishGeneration]
stepFish fish =
  map
    (\(MkFishGeneration number age) ->
       MkFishGeneration number (modularDecrement age)) $
  newFish : fish
  where
    numZeros =
      sum $ map _number $ filter (\(MkFishGeneration _ age) -> age == 0) fish
    newFish = MkFishGeneration numZeros 9
