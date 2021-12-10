module Solutions.Day10
  ( aoc10
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.Foldable       (Foldable (foldl'))
import           Data.List           (sort)
import           Data.Maybe          (isNothing, mapMaybe)
import           Text.Trifecta       (CharParsing (anyChar), Err (_expected),
                                      Parser, some)

aoc10 :: IO ()
aoc10 = do
  printSolutions 10 $ MkAoCSolution parseInput part1
  printSolutions 10 $ MkAoCSolution parseInput part2

data CheckState =
  MkCS
    { _expecting :: [Char]
    , _error     :: Maybe Char
    }
  deriving (Eq, Show)

parseInput :: Parser [String]
parseInput = do
  all <- some anyChar
  pure $ lines all

part1 :: [String] -> Integer
part1 lns = sum $ map getErrorScore errors
  where
    errors = mapMaybe (_error . checkLine) lns

part2 :: [String] -> Integer
part2 lns = sort completionScores !! (length completionScores `div` 2)
  where
    remaining = filter (isNothing . _error . checkLine) lns
    completionScores =
      map (totalCompletionScore . _expecting . checkLine) remaining

checkLine :: String -> CheckState
checkLine = foldl' foldFun (MkCS [] Nothing)
  where
    foldFun :: CheckState -> Char -> CheckState
    foldFun cs@(MkCS _ (Just error)) _ = cs
    foldFun cs@(MkCS [] _) char =
      let matched = matchBracket char
       in MkCS [matched] Nothing
    foldFun cs@(MkCS xs@(current:rest) _) char
      | char == current = MkCS rest Nothing
      | isOpeningBracket char = MkCS (matchBracket char : xs) Nothing
      | otherwise = MkCS xs $ Just char

matchBracket :: Char -> Char
matchBracket c =
  case c of
    '{' -> '}'
    '[' -> ']'
    '<' -> '>'
    '(' -> ')'
    _   -> error $ "unexpected char: " ++ show c

isOpeningBracket :: Char -> Bool
isOpeningBracket c = c `elem` ['{', '[', '<', '(']

getErrorScore :: Char -> Integer
getErrorScore c
  | c == ')' = 3
  | c == ']' = 57
  | c == '}' = 1197
  | c == '>' = 25137
  | otherwise = error $ "unexpected char: " ++ show c

getCompletionScore :: Char -> Integer
getCompletionScore c
  | c == ')' = 1
  | c == ']' = 2
  | c == '}' = 3
  | c == '>' = 4
  | otherwise = error $ "unexpected char: " ++ show c

totalCompletionScore :: String -> Integer
totalCompletionScore = foldl' f 0
  where
    f total ch = total * 5 + getCompletionScore ch
