module Solutions.Day12 where

import           Combinatorics       (tuples)
import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.FunctorUtils (fmap2)
import           Control.Applicative (Alternative (some), liftA3)
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Tuple          (swap)
import           GHC.Unicode         (isLower, isUpper)
import           Text.Trifecta       (CharParsing (char), Parser,
                                      TokenParsing (token), letter, newline,
                                      some)
import Common.ListUtils (allSets)

aoc12 :: IO ()
aoc12 = do
  printSolutions 12 $ MkAoCSolution parseInput part1
  printSolutions 12 $ MkAoCSolution parseInput part2

type Cave = String

type Path = [Cave]

type Connection = (Cave, Cave)

type CaveSystem = M.Map Cave [Cave]

parseInput :: Parser [Connection]
parseInput =
  some $
  token $ do
    startCave <- some letter
    char '-'
    endCave <- some letter
    pure (startCave, endCave)

part1 :: [Connection] -> Int
part1 = length . flip findPaths False . initCaveSystem

part2 :: [Connection] -> Int
part2 = length . flip findPaths True . initCaveSystem

initCaveSystem :: [Connection] -> CaveSystem
initCaveSystem paths = M.fromListWith (++) withReversed
  where
    withReversed = fmap2 (: []) $ paths ++ map swap paths

bigCave :: Cave -> Bool
bigCave = all isUpper

findPaths :: CaveSystem -> Bool -> [Path]
findPaths system allowSecondVisit = go S.empty (not allowSecondVisit) "start"
  where
    go visited visitedTwice "end" = [["end"]]
    go visited visitedTwice cave =
      map (cave :) $ concatMap visitChild $ system M.! cave
      where
        visitChild child
          | child == "start" = []
          | bigCave child = go updateVisited visitedTwice child
          | child `S.member` visited =
            if visitedTwice
              then []
              else go updateVisited True child
          | otherwise = go updateVisited visitedTwice child
        updateVisited = S.insert cave visited

findPathsKnots :: CaveSystem -> Bool -> [Path]
findPathsKnots system allowSecondVisit =
  memo M.! MkMemoKey "start" S.empty (not allowSecondVisit)
  where
    memo = M.fromList $ map go allMemoKeys
    allPossibleVisited = allSets $ M.keys system
    allMemoKeys =
      liftA3 MkMemoKey (M.keys system) allPossibleVisited [True, False]
    go mk@(MkMemoKey "end" _ _) = (mk, [["end"]])
    go mk@(MkMemoKey cave visited visitedTwice) =
      let paths = map (cave :) $ concatMap visitChild $ system M.! cave
       in (mk, paths)
      where
        visitChild child
          | child == "start" = []
          | bigCave child = lookupForChild child visitedTwice
          | child `S.member` visited =
            if visitedTwice
              then []
              else lookupForChild child True
          | otherwise = lookupForChild child visitedTwice
        lookupForChild child visitedTwice' =
          memo M.! MkMemoKey child (S.insert cave visited) visitedTwice'

data MemoKey =
  MkMemoKey
    { cave         :: Cave
    , visited      :: S.Set Cave
    , visitedTwice :: Bool
    }
  deriving (Eq, Show, Ord)
