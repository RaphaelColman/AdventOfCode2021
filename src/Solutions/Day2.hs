module Solutions.Day2
  ( aoc2
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.Foldable       (Foldable (foldl'))
import           Linear              (V2 (..))
import           Text.Trifecta       (Parser, TokenParsing (token), integer,
                                      letter, some, whiteSpace)

aoc2 :: IO ()
aoc2 = do
  printSolutions 2 $ MkAoCSolution parseInput part1
  printSolutions 2 $ MkAoCSolution parseInput part2

data Direction
  = Forward
  | Up
  | Down
  deriving (Show, Eq, Enum)

data Instruction =
  MkInstruction
    { _direction :: Direction
    , _amount    :: Integer
    }
  deriving (Show, Eq)

type Position = V2 Integer

data PositionWithAim =
  MkPositionWithAim
    { _position :: Position
    , _aim      :: Integer
    }
  deriving (Show, Eq)

parseInput :: Parser [Instruction]
parseInput = some $ token parseInstruction

parseInstruction :: Parser Instruction
parseInstruction = do
  directionString <- some letter
  direction <- getDirection directionString
  whiteSpace
  MkInstruction direction <$> integer
  where
    getDirection ds =
      case ds of
        "up"      -> pure Up
        "down"    -> pure Down
        "forward" -> pure Forward
        _         -> fail "Unexpected direction string"

part1 :: [Instruction] -> Integer
part1 = multipliedMagnitude . foldl' (flip addInstruction) (V2 0 0)

part2 :: [Instruction] -> Integer
part2 =
  multipliedMagnitude .
  _position . foldl' (flip addInstructionWithAim) (MkPositionWithAim (V2 0 0) 0)

addInstruction :: Instruction -> Position -> Position
addInstruction (MkInstruction direction amount) pos =
  case direction of
    Up      -> pos - V2 0 amount
    Down    -> pos + V2 0 amount
    Forward -> pos + V2 amount 0

addInstructionWithAim :: Instruction -> PositionWithAim -> PositionWithAim
addInstructionWithAim (MkInstruction direction amount) (MkPositionWithAim position aim) =
  case direction of
    Up      -> MkPositionWithAim position $ aim - amount
    Down    -> MkPositionWithAim position $ aim + amount
    Forward -> MkPositionWithAim (position + V2 amount (aim * amount)) aim

multipliedMagnitude :: V2 Integer -> Integer
multipliedMagnitude (V2 x y) = x * y
