module Solutions.Day9
  ( aoc9
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Geometry     (enumerateMultilineStringToVectorMap)
import           Data.List           (sort, unfoldr)
import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)
import qualified Data.Set            as S
import           Linear              (R1 (_x), R2 (_y), V2, unit)
import           Linear.V2           (R1 (_x), R2 (_y), V2)
import           Text.Trifecta       (CharParsing (anyChar), Parser,
                                      TokenParsing (token), digit, some)

aoc9 :: IO ()
aoc9 = do
  printSolutions 9 $ MkAoCSolution parseInput part1
  printSolutions 9 $ MkAoCSolution parseInput part2

type Grid = M.Map (V2 Int) Int

parseInput :: Parser [Char]
parseInput = some anyChar

initMap :: String -> Grid
initMap str = M.map (\c -> read [c]) $ enumerateMultilineStringToVectorMap str

part1 :: String -> Int
part1 = sum . map (+ 1) . M.elems . lowPoints . initMap

part2 :: String -> Int
part2 str = first * second * third
  where
    grid = initMap str
    lowPoints' = M.keys $ lowPoints grid
    basins = map (\p -> runExplore (MkState S.empty [p] grid)) lowPoints'
    first:second:third:rest = reverse $ sort $ map S.size basins

lowPoints :: Grid -> Grid
lowPoints grid = lowPoints
  where
    lowPoints = M.filterWithKey isLowPoint grid
    isLowPoint coord value =
      let adjacents = mapMaybe (`M.lookup` grid) $ allAdjacents coord
       in value < minimum adjacents

allDirections :: [V2 Int]
allDirections = [unit _x, -unit _x, unit _y, -unit _y]

allAdjacents :: V2 Int -> [V2 Int]
allAdjacents v = map (v +) allDirections

data ExploreState =
  MkState
    { _searched :: S.Set (V2 Int)
    , _toSearch :: [V2 Int]
    , _grid     :: Grid
    }
  deriving (Eq, Show)

explore :: Grid -> V2 Int -> [V2 Int]
explore grid coord = coord : filter passes (allAdjacents coord)
  where
    passes adjCoord =
      M.member adjCoord grid &&
      grid M.! adjCoord > grid M.! coord && grid M.! adjCoord /= 9

stepExplore :: ExploreState -> ExploreState
stepExplore (MkState searched toSearch grid) =
  MkState newSearched newToSearch grid
  where
    found = S.fromList $ concatMap (explore grid) toSearch
    newToSearch = S.toList $ S.difference found searched
    newSearched = S.union searched found

runExplore :: ExploreState -> S.Set (V2 Int)
runExplore state@(MkState searched toSearch grid)
  | null toSearch = searched
  | otherwise = runExplore $ stepExplore state

adjacents :: M.Map (V2 Int) Int -> V2 Int -> [Int]
adjacents grid c = mapMaybe (`M.lookup` grid) $ allAdjacents c
