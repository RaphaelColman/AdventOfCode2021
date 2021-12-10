module Solutions.Day10
  ( aoc10
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.Either         (lefts, rights)
import           Data.Foldable       (Foldable (foldl'), foldlM)
import           Data.List           (sort)
import qualified Data.Map            as M
import           Data.Maybe          (isNothing, mapMaybe)
import           Text.Trifecta       (CharParsing (anyChar), Err (_expected),
                                      Parser, some)

aoc10 :: IO ()
aoc10 = do
  printSolutions 10 $ MkAoCSolution parseInput part1
  printSolutions 10 $ MkAoCSolution parseInput part2

parseInput :: Parser [String]
parseInput = do
  all <- some anyChar
  pure $ lines all

part1 :: [String] -> Integer
part1 = sum . map getErrorScore . lefts . map checkLine

part2 :: [String] -> Integer
part2 lns = sort completionScores !! (length completionScores `div` 2)
  where
    remaining = rights $ map checkLine lns
    completionScores = map totalCompletionScore remaining

checkLine :: String -> Either Char [Char]
checkLine = foldlM go []
  where go [] currentChar = Right [matchBracket currentChar]
        go xs@(expected:rest) currentChar
          | currentChar == expected = Right rest
          | isOpeningBracket currentChar = Right $ matchBracket currentChar : xs
          | otherwise = Left currentChar

matchBracket :: Char -> Char
matchBracket c = M.fromList (zip "{[<(" "}]>)") M.! c

isOpeningBracket :: Char -> Bool
isOpeningBracket c = c `elem` "{[<("

getErrorScore :: Char -> Integer
getErrorScore c = M.fromList (zip ")]}>" [3, 57, 1197, 25137]) M.! c

getCompletionScore :: Char -> Integer
getCompletionScore c = M.fromList (zip ")]}>" [1, 2, 3, 4]) M.! c

totalCompletionScore :: String -> Integer
totalCompletionScore = foldl' f 0
  where
    f total ch = total * 5 + getCompletionScore ch
