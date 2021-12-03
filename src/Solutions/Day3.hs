module Solutions.Day3 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.Foldable       (Foldable (toList))
import           Data.List           (group, sort, transpose)
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as S
import           GHC.Natural         (isValidNatural)
import           Text.Trifecta       (Parser, TokenParsing (token), digit, some)

aoc3 :: IO ()
aoc3 = do
  printSolutions 3 $ MkAoCSolution parseInput part1 part2

data ReadState =
  MkReadState
    { _index  :: Int
    , _values :: [Seq BinaryDigit]
    }
  deriving (Eq, Show)

data BinaryDigit
  = ZERO
  | ONE
  deriving (Eq, Show, Enum, Ord)

parseInput :: Parser [[BinaryDigit]]
parseInput = some $ token parseBinaryDigits

parseBinaryDigits :: Parser [BinaryDigit]
parseBinaryDigits =
  some $ do
    c <- digit
    getBinary c
  where
    getBinary char =
      case char of
        '0' -> pure ZERO
        '1' -> pure ONE
        _   -> fail $ "Unexpected character" ++ [char]

part1 :: [[BinaryDigit]] -> Integer
part1 bd = gamma * epsilon
  where
    gamma = toDecimal $ map mostCommon $ transpose bd
    epsilon = toDecimal $ map leastCommon $ transpose bd

part2 :: [[BinaryDigit]] -> Maybe Integer
part2 bd = do
  let rs = initReadState bd
  oxyGen <- doReadState mostCommon rs
  let oxyGenValue = extractReadStateResult oxyGen
  co2Scrub <- doReadState leastCommon rs
  let co2ScrubValue = extractReadStateResult co2Scrub
  pure $ oxyGenValue * co2ScrubValue

initReadState :: [[BinaryDigit]] -> ReadState
initReadState input = MkReadState 0 $ map S.fromList input

stepReadState :: ([BinaryDigit] -> BinaryDigit) -> ReadState -> Maybe ReadState
stepReadState f (MkReadState index values) = do
  digits <- mapM (S.lookup index) values
  let n = f digits
  let newValues =
        filter
          (\x ->
             let lookedUp = S.index x index
              in n == lookedUp)
          values
  pure $ MkReadState (index + 1) newValues

doReadState :: ([BinaryDigit] -> BinaryDigit) -> ReadState -> Maybe ReadState
doReadState f rs =
  let stepped = stepReadState f rs
   in case stepped of
        Just readState -> doReadState f readState
        Nothing        -> pure rs

extractReadStateResult :: ReadState -> Integer
extractReadStateResult (MkReadState _ values) =
  let only = head values
   in toDecimal $ toList only

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = snd . minimum . map (\xs -> (length xs, head xs)) . group . sort

toDecimal :: [BinaryDigit] -> Integer
toDecimal bd = sum $ zipWith (*) (reverse asIntList) [2 ^ n | n <- [0,1 ..]]
  where
    asIntList = map chToInt bd
    chToInt ch =
      case ch of
        ZERO -> 0
        ONE  -> 1
