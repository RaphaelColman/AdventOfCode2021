module Solutions.Day20
  ( aoc20
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Geometry     (Grid, Point,
                                      enumerateMultilineStringToVectorMap,
                                      gridNeighbours, neighbours)
import           Control.Lens        ((&))
import           Data.List           (maximumBy, minimumBy, nub, sort, sortBy)
import qualified Data.Map            as M
import qualified Data.Map.Lazy       as M
import qualified Data.Sequence       as Seq
import qualified Data.Set            as S
import           Linear              (V2 (V2))
import           Text.Trifecta       (CharParsing (anyChar, string), Parser,
                                      Parsing (try), newline, some, whiteSpace)

aoc20 :: IO ()
aoc20 = do
  printSolutions 20 $ MkAoCSolution parseInput part1
  printSolutions 20 $ MkAoCSolution parseInput part2

type IEA = S.Set Integer

initIEA :: [Pixel] -> IEA
initIEA pixels =
  S.fromList $ map snd . filter ((== LIGHT) . fst) $ zip pixels [0 ..]

data Pixel
  = DARK
  | LIGHT
  deriving (Enum, Ord, Show, Eq)

type LightPixels = S.Set (V2 Int)

type Input = (Grid Pixel, IEA)

data ImageState =
  MkState
    { _grid      :: Grid Pixel
    , _iea       :: IEA
    , _iteration :: Integer
    }
  deriving (Eq, Show)

parseInput :: Parser Input
parseInput = do
  iea <- some $ try parsePixel
  whiteSpace
  rest <- some anyChar
  let grid =
        M.map (either error id . mapPixel) $
        enumerateMultilineStringToVectorMap rest
  pure (grid, initIEA iea)
  where
    parsePixel = do
      p <- mapPixel <$> anyChar
      either fail pure p
    mapPixel p =
      case p of
        '#' -> Right LIGHT
        '.' -> Right DARK
        _   -> Left $ "unexpected character: " ++ [p]

part1 :: Input -> Int
part1 input = count
  where
    result = runInput 2 input
    count = length $ M.filter (== LIGHT) result

part2 :: Input -> Int
part2 input = count
  where
    result = runInput 50 input
    count = length $ M.filter (== LIGHT) result

enhance :: Pixel -> Grid Pixel -> IEA -> V2 Int -> Pixel
enhance default' grid iea point =
  if decimal `S.member` iea
    then LIGHT
    else DARK
  where
    pts = sortBy orderPoints $ S.toList $ S.insert point (neighbours point)
    decimal = toDecimal $ map (flip (M.findWithDefault default') grid) pts

orderPoints :: Point -> Point -> Ordering
orderPoints (V2 x1 y1) (V2 x2 y2) =
  case compare y1 y2 of
    EQ -> compare x1 x2
    o  -> o

toDecimal :: (Enum e) => [e] -> Integer
toDecimal = sum . (zipWith (*) [2 ^ n | n <- [0,1 ..]]) . reverse . asIntList
  where
    asIntList :: (Enum e) => [e] -> [Integer]
    asIntList = map (toInteger . fromEnum)

stepImageState :: ImageState -> ImageState
stepImageState (MkState grid iea iteration) =
  ($!) MkState enhanced iea (iteration + 1)
  where
    enhanced = M.mapWithKey enhance' $ expand default' grid
    enhance' point _ = enhance default' grid iea point
    default' =
      if even iteration
        then DARK
        else LIGHT

runInput :: Int -> Input -> Grid Pixel
runInput times (grid, iea) =
  _grid $ iterate stepImageState initialState !! times
  where
    initialState = MkState grid iea 0

expand :: Pixel -> Grid Pixel -> Grid Pixel
expand default' grid =
  M.fromList $ map (\p -> (p, M.findWithDefault default' p grid)) newRange
  where
    keys = M.keysSet grid
    (V2 xtl ytl) = minimumBy orderPoints keys
    (V2 xbr ybr) = maximumBy orderPoints keys
    newRange = [V2 x y | x <- [xtl - 2 .. xbr + 2], y <- [ytl - 2 .. ybr + 2]]

renderGrid :: Grid Pixel -> Grid Char
renderGrid = M.map toCharacter
  where
    toCharacter ch =
      case ch of
        DARK  -> '.'
        LIGHT -> '#'
