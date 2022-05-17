module Solutions.Day15
    ( aoc15
    ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Debugging    (traceLns, traceVectorMap)
import           Common.Geometry     (Grid, Point, allOrthogonalNeighbours,
                                      enumerateMultilineStringToVectorMap,
                                      gridOrthogonalNeighbours, renderVectorMap)
import           Common.GraphUtils   (dijkstra)
import           Common.MapUtils     (minimumValue)
import           Data.Char           (digitToInt)
import           Data.Foldable       (maximumBy, minimum, minimumBy)
import           Data.Function       (on)
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Linear.V2           (V2 (..))
import           Safe                (minimumMay)
import           Text.Trifecta       (CharParsing (anyChar), Parser, some)

aoc15 :: IO ()
aoc15 = do
  printSolutions 15 $ MkAoCSolution parseInput part1
  printSolutions 15 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Int)
parseInput = do
  all <- some anyChar
  let charGrid = enumerateMultilineStringToVectorMap all
  pure $ M.map (\c -> read [c]) charGrid

part1 :: Grid Int -> Maybe Integer
part1 = findBestPath

part2 :: M.Map Point Int -> Maybe Integer
part2 grid =
  let unfolded = unfoldGrid grid
   in findBestPath unfolded

findBestPath :: Grid Int -> Maybe Integer
findBestPath grid = dijkstra (V2 0 0) nbr (== bottomRight grid)
  where nbr = M.map toInteger . gridOrthogonalNeighbours grid

bottomRight :: Grid a -> V2 Int
bottomRight grid = maximumBy compareFun $ M.keysSet grid
  where
    keys = maximumBy compareFun $ M.keysSet grid
    compareFun (V2 x1 y1) (V2 x2 y2) =
      case compare x1 x2 of
        EQ     -> compare y1 y2
        result -> result

unfoldGrid :: Grid Int -> Grid Int
unfoldGrid grid = ($!) M.unions [grid, verticals]
  where
    (V2 x y) = bottomRight grid
    translateHorizontal g amount =
      M.map (addWrap amount) $ M.mapKeys (\v -> v + V2 (amount * (x + 1)) 0) g
    translateVertical g amount =
      M.map (addWrap amount) $ M.mapKeys (\v -> v + V2 0 (amount * (y + 1))) g
    horizontals = M.unions $! map (translateHorizontal grid) [0 .. 4]
    verticals = M.unions $! map (translateVertical horizontals) [0 .. 4]

addWrap :: Int -> Int -> Int
addWrap x y =
  let wrapped = (x + y) `rem` 9
   in if wrapped == 0
        then 9
        else wrapped

forRender :: Grid Int -> Grid Char
forRender = M.map (head . show)
