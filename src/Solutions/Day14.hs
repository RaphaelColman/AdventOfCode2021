{-# LANGUAGE TupleSections #-}

module Solutions.Day14
  ( aoc14
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (freqs, window2)
import           Data.List           (sort)
import qualified Data.Map            as M
import           Text.Trifecta       (CharParsing (string), Parser,
                                      TokenParsing (token), count, letter, some,
                                      whiteSpace)

aoc14 :: IO ()
aoc14 = do
  printSolutions 14 $ MkAoCSolution parseInput part1
  printSolutions 14 $ MkAoCSolution parseInput part2

type Element = String

type Rule = (String, Char)

type Pair = (Char, Char)

type Template = (Element, M.Map Pair Char)

parseInput :: Parser Template
parseInput = do
  element <- some letter
  whiteSpace
  rules <-
    some $
    token $ do
      [a, b] <- count 2 letter
      string " -> "
      insert <- letter
      pure ((a, b), insert)
  pure (element, M.fromList rules)

part1 :: Template -> Int
part1 template = last sorted - head sorted
  where
    run = runTemplateNaive 10 template
    fr = freqs run
    sorted = sort $ M.elems $ freqs run

part2 :: Template -> Int
part2 template = last sorted - head sorted
  where
    rt@(MkRT pairCount elementCount) = runTemplate 40 template
    sorted = sort $ M.elems elementCount

runTemplateNaive :: Int -> Template -> String
runTemplateNaive times (start, rules) = iterate step start !! times
  where
    step current = head current : concatMap insert (window2 current)
    insert p@(a, b) = [rules M.! p, b]

type PairCount = M.Map Pair Int

type ElementCount = M.Map Char Int

data RunningTotal =
  MkRT
    { pairCount    :: PairCount
    , elementCount :: ElementCount
    }
  deriving (Eq, Show)

initRT :: Template -> RunningTotal
initRT (start, rules) = MkRT pc $ freqs start
  where
    pc = freqs $ window2 start

runTemplate :: Int -> Template -> RunningTotal
runTemplate times template@(_, rules) =
  iterate stepRT (initRT template) !! times
  where
    stepRT :: RunningTotal -> RunningTotal
    stepRT rt@(MkRT pairCount _) = M.foldrWithKey go rt pairCount
      where
        go :: Pair -> Int -> RunningTotal -> RunningTotal
        go pair@(a, b) num (MkRT pc ec) =
          let insert = rules M.! pair
              newPairs = M.fromList $ map (, num) [(a, insert), (insert, b)]
              newElementCount = M.insertWith (+) insert num ec
              withPairRemoved = M.adjust (\p -> p - num) pair pc
              newPc = M.filter (>= 0) $ M.unionWith (+) newPairs withPairRemoved
           in MkRT newPc newElementCount
