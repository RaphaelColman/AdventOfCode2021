{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day24 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (flexibleRange)
import           Control.Applicative (Alternative (empty), (<|>))
import           Data.Char           (digitToInt, isDigit)
import           Data.Foldable       (Foldable (foldl'), find)
import qualified Data.Map            as M
import           Data.Text.Format
import           Debug.Trace
import Data.List.Split
import           Text.Trifecta       (CharParsing (anyChar, char), Parser,
                                      Parsing (try), TokenParsing (token),
                                      alphaNum, digit, integer, letter, newline,
                                      some, space)

aoc24 :: IO ()
aoc24 = do
  printSolutions 24 $ MkAoCSolution parseInput part1
  --printTestSolutions 24 $ MkAoCSolution parseInput part2

data Instr
  = Inp Char
  | Add Char Parameter
  | Mul Char Parameter
  | Div Char Parameter
  | Mod Char Parameter
  | Eql Char Parameter
  deriving (Eq, Show)

data Parameter
  = Variable Char
  | Value Integer
  deriving (Eq, Show)

data Memory =
  MkMemory
    { _mem   :: M.Map Char Integer
    , _input :: [Integer]
    }
  deriving (Eq, Show)

parseInput :: Parser [Instr]
parseInput = do
  some $ token parseInstruction

parseInstruction :: Parser Instr
parseInstruction = do
  try parseInput <|> try parseOtherInstruction
  where
    parseInput :: Parser Instr
    parseInput = do
      word <- some letter
      space
      variable <- anyChar
      case word of
        "inp" -> pure $ Inp variable
        i     -> empty
    parseOtherInstruction :: Parser Instr
    parseOtherInstruction = do
      word <- some letter
      space
      variable <- anyChar
      space
      parameter <- try (Value <$> integer) <|> try (Variable <$> letter)
      case word of
        "add" -> pure $ Add variable parameter
        "mul" -> pure $ Mul variable parameter
        "div" -> pure $ Div variable parameter
        "mod" -> pure $ Mod variable parameter
        "eql" -> pure $ Eql variable parameter
        instr -> fail $ "Unexpected instruction" ++ instr

--part1 :: [Instr] -> Maybe [Integer]
part1 instructions = applyInstructions testMem instructions
  where
    testMem = MkMemory M.empty inputs
    --inputs = toModelNumber 98491959997994
    inputs = toModelNumber 61191516111321
    --inputs = [9, 1]
    chunks = chunksOf 18 instructions
    instrPair = (chunks !! 3) ++ (chunks !! 4)
    instructionsSubsection = take (18 * length inputs) instructions
    (tested, calculated) = (applyInstructions testMem instructionsSubsection, calculateZ inputs)

--instr 76 is the first mod with results that matter I think
part2 a = "unimplemented"

calculateZ inputs = expansion + coefficent
  where
    powers n = reverse [26 ^ n | n <- [0 .. (n - 1)]]
    constants = [2, 16, 9, 0, 1, 12, 6, 6, 3, 5, 9, 3, 2, 3]
    l = length inputs
    coefficent = sum $ zipWith (*) (powers l) constants
    expansion = sum $ zipWith (*) (powers l) inputs
--Seems accurate to 4 inputs. Breaks on input 5

getComponents instructions = map extract ch
  where ch = chunksOf 18 instructions
        extract cs = (cs !! 4, cs !! 5, cs !! 15)

applyInstructions :: Memory -> [Instr] -> Memory
applyInstructions = foldl' applyInstruction

--Maybe this return Maybe Memory?
applyInstruction :: Memory -> Instr -> Memory
applyInstruction (MkMemory mem input) instr =
  case instr of
    Inp c ->
      let (first:rest) = input
       in MkMemory (M.insert c first mem) rest
    Add c pa -> operation (+) c pa
    Mul c pa -> operation (*) c pa
    Div c pa -> operation div c pa
    Mod c pa -> operation mod c pa
    Eql c pa -> operation (\a b -> toInteger (fromEnum (a == b))) c pa
  where
    operation op c pa =
      let resolved = resolveParameter pa
          first = M.findWithDefault 0 c mem
          newValue = op first resolved
       in MkMemory (M.insert c newValue mem) input
    resolveParameter pa =
      case pa of
        Variable c -> M.findWithDefault 0 c mem
        Value n    -> n

modelNumbers :: [[Integer]]
modelNumbers =
  map (map (toInteger . digitToInt)) $
  filter (not . contains0) $
  map show [99999999999999,99999999999998 .. 11111111111111]
  where
    contains0 s = '0' `elem` s

toModelNumber :: Integer -> [Integer]
toModelNumber = map (toInteger . digitToInt) . show

validEndState :: Memory -> Bool
validEndState (MkMemory mem input) = (Just 0 ==) $ M.lookup 'z' mem

testMem = MkMemory (M.fromList [('z', 1), ('w', 1)]) []
