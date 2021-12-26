module Solutions.Day20
  ( aoc20
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Geometry     (Grid, Point,
                                      enumerateMultilineStringToVectorMap,
                                      gridNeighbours, neighbours)
import           Control.Lens        ((&))
import           Data.List           (sort, sortBy)
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Linear              (V2 (V2))
import           Text.Trifecta       (CharParsing (anyChar, string), Parser,
                                      Parsing (try), newline, some, whiteSpace)

aoc20 :: IO ()
aoc20 = do
  printTestSolutions 20 $ MkAoCSolution parseInput part1
  --printTestSolutions 20 $ MkAoCSolution parseInput part2

type IEA = [Pixel]

data Pixel
  = DARK
  | LIGHT
  deriving (Enum, Ord, Show, Eq)

type Input = (Grid Pixel, IEA)

parseInput :: Parser Input
parseInput = do
  iea <- some $ try parsePixel
  whiteSpace
  rest <- some anyChar
  let grid =
        M.map (either error id . mapPixel) $
        enumerateMultilineStringToVectorMap rest
  pure (grid, iea)
  where
    parsePixel = do
      p <- mapPixel <$> anyChar
      either fail pure p
    mapPixel p =
      case p of
        '#' -> Right LIGHT
        '.' -> Right DARK
        _   -> Left $ "unexpected character: " ++ [p]

--part1 :: String -> String
part1 (grid, iea) = pixelToBinary grid (V2 2 2)

part2 :: String -> String
part2 = undefined

pixelToBinary :: M.Map Point Pixel -> V2 Int -> Integer
pixelToBinary grid point =
  toDecimal $ map (flip (M.findWithDefault DARK) grid) pts
  where
    pts = sortBy orderPoints $ S.toList $ S.insert point (neighbours point)

orderPoints :: Point -> Point -> Ordering
orderPoints (V2 x1 y1) (V2 x2 y2) =
  case compare y1 y2 of
    EQ -> compare x1 x2
    o  -> o

toDecimal :: [Pixel] -> Integer
toDecimal = sum . (zipWith (*) [2 ^ n | n <- [0,1 ..]]) . reverse . asIntList
  where
    asIntList :: [Pixel] -> [Integer]
    asIntList = map (toInteger . fromEnum)
