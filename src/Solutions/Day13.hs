module Solutions.Day13
  ( aoc13
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Debugging    (traceLns, traceVectorMap)
import           Common.Geometry     (Grid, Point, renderVectorList,
                                      renderVectorSet)
import           Control.Lens        ((^.), (.~))
import           Data.List           (foldl', partition, sort)
import qualified Data.Map            as M
import           Data.Maybe          (fromJust)
import           Data.Sequence       as Seq (fromList, (!?))
import qualified Data.Set            as S
import           Debug.Trace
import Linear.V2 ( V2(..), R1(_x), R2(_y) )
import           Text.Trifecta       (CharParsing (anyChar, char, string),
                                      Parser, TokenParsing (token), commaSep,
                                      integer, letter, some, whiteSpace)

aoc13 :: IO ()
aoc13 = do
  printSolutions 13 $ MkAoCSolution parseInput part1
  printSolutions 13 $ MkAoCSolution parseInput part2

data Fold
  = YFold Int
  | XFold Int
  deriving (Eq, Show, Ord)

data Paper =
  MkPaper
    { _points :: S.Set Point
    , _folds  :: [Fold]
    }
  deriving (Eq, Show)

parseInput :: Parser Paper
parseInput = do
  points <- parsePoints
  whiteSpace
  folds <- some parseFold
  pure $ MkPaper (S.fromList points) folds

parseFold :: Parser Fold
parseFold = do
  string "fold along "
  l <- letter
  char '='
  value <- fromInteger <$> integer
  case l of
    'y' -> pure $ YFold value
    'x' -> pure $ XFold value
    _   -> fail $ "Unexpected character" ++ [l]

parsePoints :: Parser [Point]
parsePoints = do
  some $
    token $ do
      [x, y] <- commaSep integer
      pure $ fromInteger <$> V2 x y

part1 :: Paper -> Int
part1 (MkPaper points folds) = length $ applyFold (head folds) points

part2 :: Paper -> S.Set Point
part2 (MkPaper points folds) = traceLns (renderVectorSet done) done
  where
    done = foldl' (flip applyFold) points folds

applyFold :: Fold -> S.Set Point -> S.Set Point
applyFold fold points =
  case fold of
    YFold n -> doFold n inY points
    XFold n -> doFold n inX points

doFold :: Int -> Axis -> S.Set Point -> S.Set Point
doFold val (get', reflectF) points = S.union folded above
  where
    withoutLine = S.filter (\v -> get' v /= val) points
    (below, above) = S.partition (\v -> get' v > val) withoutLine
    folded = S.map (reflectF val) below

reflectX :: Int -> Point -> Point
reflectX val (V2 x y) =
  let amount = val - x
   in V2 (val + amount) y

reflectY :: Int -> Point -> Point
reflectY val (V2 x y) =
  let amount = val - y
   in V2 x (val + amount)

inX :: (V2 a -> a, Int -> Point -> Point)
inX = ((^. _x), reflectX)

inY :: (V2 a -> a, Int -> Point -> Point)
inY = ((^. _y), reflectY)

type Axis = (Point -> Int, Int -> Point -> Point)