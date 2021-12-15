module Common.Geometry where

import           Control.Lens    ((^.))
import           Data.Foldable   (maximumBy, minimumBy)
import           Data.List.Split (chunksOf)
import qualified Data.Map        as M hiding (mapMaybe)
import           Data.Maybe      (mapMaybe)
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq
import qualified Data.Set        as S
import           Linear.V2       (R1 (_x), R2 (_y), V2 (..))
import Linear (unit)

type Point = V2 Int

type Grid a = M.Map Point a

enumerateMultilineString :: String -> [((Int, Int), Char)]
enumerateMultilineString str
  | maximum lengths /= minimum lengths = error "Line lengths are not equal"
  | otherwise = zip coords (concat lines')
  where
    lines' = lines str
    xLength = length (head lines')
    yLength = length lines'
    lengths = map length lines'
    coords = [(x, y) | y <- [0 .. yLength - 1], x <- [0 .. xLength - 1]]

enumerateMultilineStringToVectorMap :: String -> M.Map (V2 Int) Char
enumerateMultilineStringToVectorMap =
  M.fromList . map (\((x, y), c) -> (V2 x y, c)) . enumerateMultilineString

gridNeighbours :: Grid a -> Point -> M.Map Point a
gridNeighbours grid point = M.restrictKeys grid $ neighbours point

neighbours :: Point -> S.Set Point
neighbours point = S.fromList $ map (+ point) directions
  where
    directions = [V2 x y | x <- units, y <- units, [x, y] /= [0, 0]]
    units = [-1, 0, 1]

renderVectorMap :: M.Map (V2 Int) Char -> String
renderVectorMap m =
  if null m
    then ""
    else rendered
  where
    keys = M.keys m
    xMax = maximumBy (\a b -> compare (a ^. _x) (b ^. _x)) keys ^. _x
    xMin = minimumBy (\a b -> compare (a ^. _x) (b ^. _x)) keys ^. _x
    yMax = maximumBy (\a b -> compare (a ^. _y) (b ^. _y)) keys ^. _y
    yMin = minimumBy (\a b -> compare (a ^. _y) (b ^. _y)) keys ^. _y
    xRange = (xMax - xMin) + 1
    panelList =
      [ M.findWithDefault '.' (V2 x y) m
      | y <- [yMin .. yMax]
      , x <- [xMin .. xMax]
      ]
    panelRows = chunksOf xRange panelList
    rendered = unlines (replicate xRange '=' : panelRows)

renderVectorSet :: S.Set Point -> String
renderVectorSet points =
  let asMap = M.fromSet (const '#') points
   in renderVectorMap asMap

renderVectorList :: [Point] -> String
renderVectorList = renderVectorSet . S.fromList 

allOrthogonalDirections :: [V2 Int]
allOrthogonalDirections = [unit _x, -unit _x, unit _y, -unit _y]

allOrthogonalNeighbours :: V2 Int -> S.Set Point
allOrthogonalNeighbours v = S.fromList $ map (v +) allOrthogonalDirections

gridOrthogonalNeighbours :: Grid a -> Point -> M.Map Point a
gridOrthogonalNeighbours grid point = M.restrictKeys grid $ allOrthogonalNeighbours point