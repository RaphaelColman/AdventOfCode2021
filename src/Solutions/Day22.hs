module Solutions.Day22
  ( aoc22
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import qualified Common.SetUtils     as SU
import           Control.Applicative (Alternative (some), (<|>))
import           Control.Lens        ((&))
import           Data.List           (foldl')
import           Data.Maybe          (fromJust, isJust, mapMaybe)
import qualified Data.Set            as S
import           Linear              (V3 (V3))
import           Text.Trifecta       (CharParsing (anyChar, char, string),
                                      Parser, Parsing (try), commaSep, integer,
                                      sepBy, symbol, whiteSpace)

aoc22 :: IO ()
aoc22 = do
  printSolutions 22 $ MkAoCSolution parseInput part1
  printSolutions 22 $ MkAoCSolution parseInput part2

type Point = V3 Integer

type PointRange = (Integer, Integer)

data Instruction =
  MkInstruction
    { _on     :: Bool
    , _cuboid :: Cuboid
    }
  deriving (Eq, Show, Ord)

data Cuboid =
  MkCuboid
    { _xRange :: PointRange
    , _yRange :: PointRange
    , _zRange :: PointRange
    }
  deriving (Eq, Show, Ord)

parseInput :: Parser [Instruction]
parseInput = some parseInstruction

parseInstruction :: Parser Instruction
parseInstruction = do
  switch <- (== "on") <$> (try (string "on") <|> try (string "off"))
  whiteSpace
  [xRange, yRange, zRange] <- commaSep parseRange
  pure $ MkInstruction switch $ MkCuboid xRange yRange zRange
  where
    parseRange :: Parser PointRange
    parseRange = do
      anyChar >> char '='
      [min, max] <- integer `sepBy` symbol ".."
      pure (min, max)

part1 :: [Instruction] -> Integer
part1 instructions = runReboot filtered
  where
    filtered = filter (isJust . intersection target . _cuboid) instructions
    target = MkCuboid (-50, 50) (-50, 50) (-50, 50)

part2 :: [Instruction] -> Integer
part2 = runReboot

area :: Cuboid -> Integer
area (MkCuboid (x1, x2) (y1, y2) (z1, z2)) = xLength * yLength * zLength
  where
    xLength = abs (x1 - x2) + 1
    yLength = abs (y1 - y2) + 1
    zLength = abs (z1 - z2) + 1

intersection :: Cuboid -> Cuboid -> Maybe Cuboid
intersection (MkCuboid (xLeft1, xRight1) (yNear1, yFar1) (zBottom1, zTop1)) (MkCuboid (xLeft2, xRight2) (yNear2, yFar2) (zBottom2, zTop2))
  | bottom > top = Nothing
  | left > right = Nothing
  | near > far = Nothing
  | otherwise = Just $ MkCuboid (left, right) (near, far) (bottom, top)
  where
    bottom = max zBottom1 zBottom2
    top = min zTop1 zTop2
    left = max xLeft1 xLeft2
    right = min xRight1 xRight2
    near = max yNear1 yNear2
    far = min yFar1 yFar2

type RebootState = [Instruction]

deriveExtraInstruction :: Instruction -> Instruction -> Maybe Instruction
deriveExtraInstruction (MkInstruction _ cuboid1) (MkInstruction on cuboid2) =
  MkInstruction (not on) <$> intersection cuboid1 cuboid2

applyInstruction :: RebootState -> Instruction -> RebootState
applyInstruction instructions instruction@(MkInstruction on _)
  | on = instruction : newInstructions ++ instructions
  | otherwise = instructions ++ newInstructions
  where
    newInstructions = mapMaybe (deriveExtraInstruction instruction) instructions

deriveArea :: RebootState -> Integer
deriveArea = foldr go 0
  where
    go (MkInstruction on cuboid) total
      | on = total + area cuboid
      | otherwise = total - area cuboid

runReboot :: [Instruction] -> Integer
runReboot instructions = deriveArea $ foldl' applyInstruction [] instructions
