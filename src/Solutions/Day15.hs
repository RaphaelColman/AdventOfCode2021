module Solutions.Day15
  ( aoc15
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Geometry     (Grid, Point, allOrthogonalNeighbours,
                                      enumerateMultilineStringToVectorMap,
                                      gridOrthogonalNeighbours)
import           Control.Lens        ((.~), (^.))
import           Data.Char           (digitToInt)
import           Data.Foldable       (maximumBy, minimum, minimumBy)
import           Data.Function       (on)
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S
import           Debug.Trace
import           Linear.V2
import           Safe                (minimumMay)
import           Text.Trifecta       (CharParsing (anyChar), Parser, some)

aoc15 :: IO ()
aoc15 = do
  printSolutions 15 $ MkAoCSolution parseInput part1
  --printSolutions 15 $ MkAoCSolution parseInput part2

parseInput :: Parser (Grid Int)
parseInput = do
  all <- some anyChar
  let charGrid = enumerateMultilineStringToVectorMap all
  pure $ M.map (\c -> read [c]) charGrid

part1 :: Grid Int -> Int
part1 = dijkstra

part2 :: String -> String
part2 = undefined

dijkstra :: Grid Int -> Int
dijkstra grid = go (V2 0 0) (M.fromList [(V2 0 0, 0)]) S.empty
  where
    bottomRight' = bottomRight grid
    go current tDistances visited
      | current == bottomRight' = tDistances M.! current
      | otherwise = go minNode newTDistances (S.insert current newVisited)
      where
        children = gridOrthogonalNeighbours grid current
        unVisitedChildren =
          M.filterWithKey (\point _ -> not (point `S.member` visited)) children
        distances = M.map (+ tDistances M.! current) unVisitedChildren
        newTDistances = M.unionWith min distances tDistances
        newVisited = S.insert current visited
        minNode =
          fst $
          minimumBy (compare `on` snd) $
          M.toList $
          M.filterWithKey (\k a -> not (k `S.member` newVisited)) newTDistances

bottomRight :: Grid a -> V2 Int
bottomRight grid = maximumBy compareFun $ M.keysSet grid
  where
    keys = maximumBy compareFun $ M.keysSet grid
    compareFun (V2 x1 y1) (V2 x2 y2) =
      case compare x1 x2 of
        EQ     -> compare y1 y2
        result -> result
