module Solutions.Day10
  ( aoc10
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Data.Foldable       (Foldable (foldl'))
import           Data.List           (sort)
import qualified Data.Map            as M
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
