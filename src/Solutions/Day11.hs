module Solutions.Day11
  ( aoc11
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Debugging    (traceLns, traceVectorMap)
import           Common.Geometry     (Grid, Point,
                                      enumerateMultilineStringToVectorMap,
                                      gridNeighbours, renderVectorMap,
                                      renderVectorSet)
import           Common.ListUtils    (freqs)
import           Data.List           (findIndex)
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import           Text.Trifecta       (CharParsing (anyChar), Parser, some)

aoc11 :: IO ()
aoc11 = do
  printSolutions 11 $ MkAoCSolution parseInput part1
  printSolutions 11 $ MkAoCSolution parseInput part2

type Octopodes = Grid Integer

type FreqMap = M.Map Point Integer

data FlashTracker =
  MkFT
    { _alreadFlashed :: S.Set Point
    , _newFlashes    :: S.Set Point
    , _octopodes     :: Octopodes
    }
  deriving (Eq, Show)

parseInput :: Parser Octopodes
parseInput = do
  allChars <- some anyChar
  let charMap = enumerateMultilineStringToVectorMap allChars
  pure $ M.map (\c -> read [c]) charMap

part1 :: Octopodes -> Int
part1 = sum . map countZero . take 101 . iterate step

part2 :: Octopodes -> Maybe Int
part2 = findIndex allZero . iterate step

countZero :: Octopodes -> Int
countZero = length . M.filter (== 0)

allZero :: Octopodes -> Bool
allZero = all (== 0) . M.elems

flashing :: Octopodes -> S.Set Point
flashing = M.keysSet . M.filter (> 9)

grow :: Octopodes -> Octopodes
grow = M.map (+ 1)

step :: Octopodes -> Octopodes
step octopodes =
  M.map
    (\a ->
       if a > 9
         then 0
         else a)
    flashed
  where
    flashed = runFlash $ grow octopodes

runFlash :: Octopodes -> Octopodes
runFlash octopodes = go $ MkFT S.empty (flashing octopodes) octopodes
  where
    go :: FlashTracker -> Octopodes
    go ft@(MkFT alreadyFlashed newFlashes octopodes')
      | null newFlashes = grown
      | otherwise = go (MkFT newAlreadyFlashed newNewFlashes grown)
      where
        freqMap =
          freqs . concatMap (M.keys . gridNeighbours octopodes') $ newFlashes
        grown = M.unionWith (+) freqMap octopodes'
        newAlreadyFlashed = S.union alreadyFlashed newFlashes
        newNewFlashes =
          M.keysSet $
          M.filterWithKey
            (\k a -> a > 9 && S.notMember k newAlreadyFlashed)
            grown
