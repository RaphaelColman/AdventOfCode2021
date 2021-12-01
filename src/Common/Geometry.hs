{-# LANGUAGE TupleSections #-}

module Common.Geometry where

import           Control.Lens    ((^.))
import           Data.Foldable   (maximumBy, minimumBy)
import           Data.List.Split (chunksOf)
import qualified Data.Map        as M
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq
import           Linear.V2       (R1 (_x), R2 (_y), V2 (..))

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

freqs :: (Ord k, Num a) => [k] -> M.Map k a
freqs xs = M.fromListWith (+) (map (, 1) xs)

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

enumNext :: (Enum a, Eq a, Bounded a) => a -> a
enumNext e
  | e == maxBound = minBound
  | otherwise = succ e

enumPrev :: (Enum a, Eq a, Bounded a) => a -> a
enumPrev e
  | e == minBound = maxBound
  | otherwise = pred e

stepEnum :: (Enum a, Eq a, Bounded a) => a -> Int -> a
stepEnum enum times = iterate enumNext enum !! times

prevStepEnum :: (Enum a, Eq a, Bounded a) => a -> Int -> a
prevStepEnum enum times = iterate enumPrev enum !! times
