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
import           Common.MapUtils     (mapIf)
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
    , _flashes       :: S.Set Point
    , _octopodes     :: Octopodes
    }
  deriving (Eq, Show)

parseInput :: Parser Octopodes
parseInput = do
  allChars <- some anyChar
  let charMap = enumerateMultilineStringToVectorMap allChars
  pure $ M.map (\c -> read [c]) charMap

part1 :: Octopodes -> Int
part1 = sum . map (length . M.filter (== 0)) . take 101 . iterate step

part2 :: Octopodes -> Maybe Int
part2 = findIndex (all (== 0) . M.elems) . iterate step

flashing :: Octopodes -> S.Set Point
flashing = M.keysSet . M.filter (> 9)

grow :: Octopodes -> Octopodes
grow = M.map (+ 1)

step :: Octopodes -> Octopodes
step = mapIf (> 9) (const 0) . runFlash . grow

runFlash :: Octopodes -> Octopodes
runFlash octopodes = go $ MkFT S.empty (flashing octopodes) octopodes
  where
    go :: FlashTracker -> Octopodes
    go ft@(MkFT alreadyFlashed flashes octopodes')
      | null flashes = grown
      | otherwise = go (MkFT newAlreadyFlashed newFlashes grown)
      where
        freqMap =
          freqs . concatMap (M.keys . gridNeighbours octopodes') $ flashes
        grown = M.unionWith (+) freqMap octopodes'
        newAlreadyFlashed = S.union alreadyFlashed flashes
        newFlashes = S.difference (flashing grown) newAlreadyFlashed

convertForRender :: Octopodes -> M.Map Point Char
convertForRender = M.map (head . show)
