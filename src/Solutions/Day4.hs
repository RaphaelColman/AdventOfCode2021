{-# LANGUAGE TupleSections #-}

module Solutions.Day4
  ( aoc4
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.FunctorUtils (fmap3)
import           Control.Monad       (filterM)
import           Data.List           (find, partition, transpose)
import           Text.Trifecta       (Parser, commaSep, count, integer, some,
                                      whiteSpace)

aoc4 :: IO ()
aoc4 = do
  printSolutions 4 $ MkAoCSolution parseInput part1 part2

type Board = [[BingoSquare]]

type Numbers = [Integer]

type BingoSquare = (Integer, Bool)

data Bingo =
  MkBingo
    { _boards        :: [Board]
    , _numbers       :: Numbers
    , _calledNumbers :: Numbers
    , _winners       :: [Board]
    }
  deriving (Eq, Show)

parseInput :: Parser Bingo
parseInput = do
  nums <- commaSep integer
  whiteSpace
  boardNumbers <- some $ count 5 $ count 5 integer
  let boards = fmap3 (, False) boardNumbers
  pure $ MkBingo boards nums [] []

part1 :: Bingo -> Maybe Integer
part1 = runBingo

part2 :: Bingo -> Maybe Integer
part2 = runBingoUntilLast

stepBingo :: Bingo -> Maybe Bingo
stepBingo bingo@(MkBingo boards numbers calledNumbers winners)
  | null numbers = Nothing
  | otherwise = do
    let newBoards = fmap3 checkSquare boards
        checkSquare sq@(n, checked) =
          if checked
            then sq
            else (n, n == head numbers)
    let (newWinners, losers) = partition checkBoard newBoards
    pure $
      MkBingo
        losers
        (tail numbers)
        (head numbers : calledNumbers)
        (newWinners ++ winners)

checkBoard :: Board -> Bool
checkBoard board =
  let rowsComplete = any (all snd) board
      columnsComplete = any (all snd) $ transpose board
   in rowsComplete || columnsComplete

runBingo :: Bingo -> Maybe Integer
runBingo bingo@(MkBingo boards numbers calledNumbers winners)
  | null winners = stepBingo bingo >>= runBingo
  | otherwise = pure $ evaluateBoard (head winners) (head calledNumbers)

runBingoUntilLast :: Bingo -> Maybe Integer
runBingoUntilLast bingo@(MkBingo boards numbers calledNumbers winners)
  | null boards = pure $ evaluateBoard (head winners) (head calledNumbers)
  | otherwise = stepBingo bingo >>= runBingoUntilLast

evaluateBoard :: Board -> Integer -> Integer
evaluateBoard board lastCalled =
  let unmarkedSum = sum $ map fst $ filter (not . snd) $ concat board
   in unmarkedSum * lastCalled