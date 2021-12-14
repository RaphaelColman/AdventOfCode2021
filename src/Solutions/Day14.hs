{-# LANGUAGE TupleSections #-}

module Solutions.Day14
  ( aoc14
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (freqs, leastCommon, mostCommon, window2)
import           Common.MapUtils     (mapIf)
import           Control.Arrow       (Arrow (first))
import           Data.Foldable       (maximumBy, minimumBy)
import           Data.Function       (on)
import           Data.List           (sort)
import           Data.Map            ((!?))
import qualified Data.Map            as M
import           Text.Trifecta       (CharParsing (string), Parser,
                                      TokenParsing (token), count, letter, some,
                                      whiteSpace)
import Debug.Trace
import Common.Debugging (traceShowIf)

aoc14 :: IO ()
aoc14 = do
  printSolutions 14 $ MkAoCSolution parseInput part1
  printSolutions 14 $ MkAoCSolution parseInput part2

type Element = String

type Rule = (Element, Char)

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

--part1 :: Template -> Int
part1 template = mTimes - lTimes
  where
    run = runTemplate 10 template
    fr = freqs run
    (most, mTimes) = maximumBy (compare `on` snd) $ M.toList fr
    (least, lTimes) = minimumBy (compare `on` snd) $ M.toList fr

--part2 :: Template -> [(Char, Char)]
part2 template = mTimes - lTimes
  where
    rt@(MkRT pairCount elementCount) = runTemplate2 40 template
    (most, mTimes) = maximumBy (compare `on` snd) $ M.toList elementCount
    (least, lTimes) = minimumBy (compare `on` snd) $ M.toList elementCount


--Different for real input after step 1. It should have 5 'O's but only has 4
-- It think's there's 1 NB but there are 2...

runTemplate :: Int -> Template -> String
runTemplate times (start, rules) = iterate step start !! times
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

runTemplate2 :: Int -> Template -> RunningTotal
runTemplate2 times template@(_, rules) =
  iterate stepRT (initRT template) !! times
  where
    stepRT :: RunningTotal -> RunningTotal
    stepRT rt@(MkRT pairCount elementCount) = M.foldrWithKey go rt pairCount
      where
        go :: Pair -> Int -> RunningTotal -> RunningTotal
        go pair@(a, b) num rt@(MkRT pc ec) =
          let insert = rules M.! pair
              newPairs = M.fromList $ map (, num) [(a, insert), (insert, b)]
              newElementCount = M.insertWith (+) insert num ec
              withPairRemoved = M.adjust (\p -> p - num) pair pc
              newPc = M.filter (>= 0) $ M.unionWith (+) newPairs withPairRemoved
           in MkRT newPc newElementCount

type Counter = M.Map Pair Int

type Rules = M.Map Pair [Pair]