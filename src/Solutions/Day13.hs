module Solutions.Day13
  ( aoc13
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Debugging    (traceLns, traceVectorMap)
import           Common.Geometry     (Point, renderVectorList, Grid)
import           Control.Lens        ((^.))
import           Data.List           (partition, sort, foldl')
import qualified Data.Map            as M
import           Data.Maybe          (fromJust)
import           Data.Sequence       as Seq (fromList, (!?))
import qualified Data.Set            as S
import           Debug.Trace
import           Linear.V2
import           Text.Trifecta       (CharParsing (anyChar, char, string),
                                      Parser, TokenParsing (token), commaSep,
                                      integer, letter, some, whiteSpace)

aoc13 :: IO ()
aoc13 = do
  printSolutions 13 $ MkAoCSolution parseInput part1
  printSolutions 13 $ MkAoCSolution parseInput part2

data Fold
  = Horizontal Int
  | Vertical Int
  deriving (Eq, Show, Ord)

data Paper =
  MkPaper
    { _points :: [Point]
    , _folds  :: [Fold]
    }
  deriving (Eq, Show)

parseInput :: Parser Paper
parseInput = do
  points <- parsePoints
  whiteSpace
  folds <- some parseFold
  pure $ MkPaper points folds

parseFold :: Parser Fold
parseFold = do
  string "fold along "
  l <- letter
  char '='
  value <- fromInteger <$> integer
  case l of
    'y' -> pure $ Horizontal value
    'x' -> pure $ Vertical value
    _   -> fail $ "Unexpected character" ++ [l]

parsePoints :: Parser [Point]
parsePoints = do
  some $
    token $ do
      [x, y] <- commaSep integer
      pure $ fromInteger <$> V2 x y

part1 :: Paper -> Int
part1 (MkPaper points folds) = length $ applyFold (head folds) points

part2 :: Paper -> [Point]
part2 (MkPaper points folds) = traceLns (renderVectorList done) done
  where done = foldl' (flip applyFold) points folds

applyFold :: Fold -> [Point] -> [Point]
applyFold fold points = case fold of
  Horizontal n -> foldAlongHorizontal n points
  Vertical n -> foldAlongVertical n points


foldAlongHorizontal :: Int -> [Point] -> [Point]
foldAlongHorizontal val points = S.toList merged
  where
    withoutLine = filter (\(V2 _ y) -> y /= val) points
    (below, above) = partition (\(V2 _ y) -> y > val) withoutLine
    foldedBottom = flipVertical (val * 2) below
    merged = S.union (S.fromList foldedBottom) (S.fromList above)

foldAlongVertical :: Int -> [Point] -> [Point]
foldAlongVertical val points = S.toList merged
  where
    withoutLine = filter (\(V2 x _) -> x /= val) points
    (right, left) = partition (\(V2 x _) -> x > val) withoutLine
    foldedRight = flipHorizontal (val * 2) right
    merged = S.union (S.fromList foldedRight) (S.fromList left)

flipVertical :: Int -> [Point] -> [Point]
flipVertical span = map flipPoint
  where
    flipNum num = fromJust $ Seq.fromList [span,span - 1 .. 0] Seq.!? num
    flipPoint (V2 x y) = V2 x (flipNum y)

flipHorizontal :: Int -> [Point] -> [Point]
flipHorizontal span = map flipPoint
  where
    flipNum num = fromJust $ Seq.fromList [span,span - 1 .. 0] Seq.!? num
    flipPoint (V2 x y) = V2 (flipNum x) y
