module Solutions.Day19 where

import           Combinatorics       (tuples)
import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Control.Monad       (msum)
import           Data.List           (find, permutations, transpose)
import           Data.Maybe          (fromJust, isJust, mapMaybe)
import qualified Data.Sequence       as Seq
import qualified Data.Set            as S
import           Debug.Trace
import           Linear              (V2 (V2))
import           Linear.V3
import           Safe                (headMay)
import           Text.Trifecta       (CharParsing (char, string), Parser,
                                      Parsing (try), commaSep, integer, newline,
                                      some, token, whiteSpace)

aoc19 :: IO ()
aoc19 = do
  printSolutions 19 $ MkAoCSolution parseInput part1
  printSolutions 19 $ MkAoCSolution parseInput part2

parseInput :: Parser [Scanner]
parseInput = some $ token parseScanner
  where
    parseScanner = do
      string "--- scanner " >> integer >> string "---"
      newline
      points <- some parsePoint
      pure $ MkScanner (S.fromList points) (V3 0 0 0)
    parsePoint :: Parser Point
    parsePoint
      --Stupid '-' delimiter means you have to try parsing it as a integer, then fail if it's actually a header
     = do
      [x, y, z] <- try $ commaSep integer
      pure $ V3 x y z

part1 :: [Scanner] -> Maybe Int
part1 scanners = do
  assembled <- assemble scanners
  pure $ length $ S.unions $ map _beacons assembled

part2 :: [Scanner] -> Maybe Integer
part2 scanners = do
  locations <- map _location <$> assemble scanners
  pure $
    maximum $ map (\[p1, p2] -> manhattanDistance p1 p2) $ tuples 2 locations

assemble :: [Scanner] -> Maybe [Scanner]
assemble scanners = do
  let (scanner0:rest) = scanners
  let queue = MkSQ [scanner0] $ Seq.fromList rest
  runQueue queue

type Point = V3 Integer

data Scanner =
  MkScanner
    { _beacons  :: S.Set Point
    , _location :: Point
    }
  deriving (Eq, Show, Ord)

--x = forward/backwards
--y = up/down
--z = left/right
allOrientations :: Num a => V3 a -> [V3 a]
allOrientations v = concatMap allRotations allFacings
  where
    backwards (V3 x y z) = V3 (-x) y (-z)
    up (V3 x y z) = V3 y (-x) z
    down (V3 x y z) = V3 (-y) x z
    left (V3 x y z) = V3 (-z) y x
    right (V3 x y z) = V3 z y (-x)
    clockwise90 (V3 x y z) = V3 x z (-y)
    clockwise180 (V3 x y z) = V3 x (-y) (-z)
    clockwise270 (V3 x y z) = V3 x (-z) y
    allFacings = map (\f -> f v) [id, backwards, up, down, left, right]
    allRotations v' =
      map (\f -> f v') [id, clockwise90, clockwise180, clockwise270]

relativeNeighbour :: Scanner -> Scanner -> Maybe Scanner
relativeNeighbour base scanner = do
  let allRotations = rotateScanner scanner
  headMay $ mapMaybe (compare' base) allRotations
  where
    compare' base'@(MkScanner basePoints baseLoc) scanner'@(MkScanner points loc) =
      let allTranspositions =
            S.map (uncurry (-)) $ S.cartesianProduct basePoints points
          allTransposedScanners =
            S.map (transposeScanner scanner') allTranspositions
       in find
            (\(MkScanner points loc) ->
               length (S.intersection points basePoints) >= 12)
            allTransposedScanners

transposeScanner :: Scanner -> V3 Integer -> Scanner
transposeScanner (MkScanner points location) transposition =
  MkScanner (S.map (+ transposition) points) (location + transposition)

rotateScanner :: Scanner -> [Scanner]
rotateScanner (MkScanner points location) =
  map (flip MkScanner (V3 0 0 0) . S.fromList) $
  transpose $ map allOrientations $ S.toList points

data ScannerQueue =
  MkSQ
    { found     :: [Scanner]
    , remaining :: Seq.Seq Scanner
    }
  deriving (Eq, Show)

runQueue :: ScannerQueue -> Maybe [Scanner]
runQueue sq@(MkSQ found remaining)
  | null remaining = pure found
  | otherwise = stepQueue sq >>= runQueue

stepQueue :: ScannerQueue -> Maybe ScannerQueue
stepQueue (MkSQ found remaining) = do
  tryScanner <- remaining Seq.!? 0
  let restOfList = Seq.drop 1 remaining
  let orientated = headMay $ mapMaybe (`relativeNeighbour` tryScanner) found
  case orientated of
    Just o  -> pure $ MkSQ (o : found) restOfList
    Nothing -> pure $ MkSQ found (restOfList Seq.|> tryScanner)

manhattanDistance :: Point -> Point -> Integer
manhattanDistance (V3 x1 y1 z1) (V3 x2 y2 z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
