module Solutions.Day25
  ( aoc25
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Debugging    (traceVectorMap)
import           Common.Geometry     (Grid, Point,
                                      enumerateMultilineStringToVectorMap,
                                      renderVectorMap)
import           Control.Lens        ((^.))
import           Data.Function       (on, (&))
import           Data.List           (maximumBy)
import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)
import qualified Data.Set            as S
import           Linear              (V2 (V2))
import           Text.Trifecta       (CharParsing (anyChar), Parser, some)

aoc25 :: IO ()
aoc25 = do
  printTestSolutions 25 $ MkAoCSolution parseInput part1

data SeaCucumber
  = East
  | South
  deriving (Eq, Show, Ord, Enum)

data Seabed =
  MkSeabed
    { _cucumbers  :: Grid SeaCucumber
    , _dimensions :: V2 Int
    }
  deriving (Eq, Show, Ord)

parseInput :: Parser Seabed
parseInput = do
  allChars <- some anyChar
  let grid =
        M.mapMaybe toSeaCucumber $ enumerateMultilineStringToVectorMap allChars
  let pointsAsList = M.toList grid
  let mx = maximum $ map (\(V2 x y, _) -> x) pointsAsList
  let my = maximum $ map (\(V2 x y, _) -> y) pointsAsList
  pure $ MkSeabed grid $ V2 mx my
  where
    toSeaCucumber ch =
      case ch of
        '>' -> Just East
        'v' -> Just South
        '.' -> Nothing
        _   -> fail "Unrecognised character"
    compareX (V2 x1 _) (V2 x2 _) = compare x1 x2
    compareY (V2 _ y1) (V2 _ y2) = compare y1 y2

part1 :: Seabed -> Int
part1 = runSeabed

runSeabed :: Seabed -> Int
runSeabed = go 1
  where
    go index sb
      | stepped == sb = index
      | otherwise = go (index + 1) stepped
      where
        stepped = step sb

step :: Seabed -> Seabed
step = stepCucumber South . stepCucumber East

stepCucumber :: SeaCucumber -> Seabed -> Seabed
stepCucumber sc (MkSeabed grid dimens@(V2 maxX maxY)) =
  MkSeabed (M.union moved rest) dimens
  where
    (movable, rest) =
      M.partitionWithKey
        (\k v -> (v == sc) && M.notMember (next k) grid)
        grid
    moved = M.mapKeys next movable
    next pt = getNext sc pt dimens

getNext :: SeaCucumber -> Point -> Point -> Point
getNext East (V2 x y) (V2 maxX maxY) =
  let next = x + 1
   in if next > maxX
        then V2 0 y
        else V2 next y
getNext South (V2 x y) (V2 maxX maxY) =
  let next = y + 1
   in if next > maxY
        then V2 x 0
        else V2 x next

renderSeabed :: Seabed -> M.Map Point Char
renderSeabed (MkSeabed grid _) = M.map toChar grid
  where
    toChar sc =
      case sc of
        East  -> '>'
        South -> 'v'

showSeabed :: Seabed -> IO ()
showSeabed = putStrLn . renderVectorMap . renderSeabed
