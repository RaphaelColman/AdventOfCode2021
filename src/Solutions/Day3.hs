module Solutions.Day3
  ( aoc3
  ) where

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
parseBinaryDigits = some $ digit >>= getBinary
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
  oxyGen <- toDecimal <$> runReadState mostCommon rs
  co2Scrub <- toDecimal <$> runReadState leastCommon rs
  pure $ oxyGen * co2Scrub

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

runReadState ::
     ([BinaryDigit] -> BinaryDigit) -> ReadState -> Maybe [BinaryDigit]
runReadState f rs@(MkReadState index values)
  | length values == 1 =
    let value = head values
     in pure $ toList value
  | otherwise = stepReadState f rs >>= runReadState f

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

leastCommon :: Ord a => [a] -> a
leastCommon = snd . minimum . map (\xs -> (length xs, head xs)) . group . sort

toDecimal :: [BinaryDigit] -> Integer
toDecimal = sum . (zipWith (*) [2 ^ n | n <- [0,1 ..]]) . reverse . asIntList
  where
    asIntList :: [BinaryDigit] -> [Integer]
    asIntList = map (toInteger . fromEnum)
