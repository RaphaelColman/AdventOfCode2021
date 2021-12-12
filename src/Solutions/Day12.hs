module Solutions.Day12 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.FunctorUtils (fmap2)
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Data.Tuple          (swap)
import           Debug.Trace
import           GHC.Unicode         (isLower, isUpper)
import           Text.Trifecta       (CharParsing (char), Parser,
                                      TokenParsing (token), letter, newline,
                                      some)

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
part1 paths = length $ findPaths caveSystem
  where
    caveSystem = initCaveSystem paths

part2 :: [Connection] -> Int
part2 = length . findPaths2 . initCaveSystem

initCaveSystem :: [Connection] -> CaveSystem
initCaveSystem paths = M.fromListWith (++) withReversed
  where
    withReversed = fmap2 (: []) $ paths ++ map swap paths

bigCave :: Cave -> Bool
bigCave = all isUpper

smallCave :: Cave -> Bool
smallCave = all isLower

findPaths :: CaveSystem -> [Path]
findPaths system = go S.empty "start"
  where
    go :: S.Set Cave -> Cave -> [Path]
    go visited "end" = [["end"]]
    go visited cave = added
      where
        children = system M.! cave
        validChildren =
          filter
            (\child -> bigCave child || not (child `S.member` visited))
            children
        childPaths = concatMap (go (S.insert cave visited)) validChildren
        added = map (cave :) childPaths

findPaths2 :: CaveSystem -> [Path]
findPaths2 system = go S.empty Nothing "start"
  where
    go :: S.Set Cave -> Maybe Cave -> Cave -> [Path]
    go visited visitedTwice "end" = [["end"]]
    go visited visitedTwice cave = added
      where
        children = system M.! cave
        validChildren =
          filter fst3 $ map (isValidChild visited visitedTwice) children
        childPaths =
          concatMap
            (\(b, vt, ch) -> go (S.insert cave visited) vt ch)
            validChildren
        added = map (cave :) childPaths

isValidChild :: S.Set Cave -> Maybe Cave -> Cave -> (Bool, Maybe Cave, Cave)
isValidChild visited visitedTwice child
  | bigCave child = (True, visitedTwice, child)
  | child == "end" = (not alreadySeen, visitedTwice, child)
  | child == "start" = (not alreadySeen, visitedTwice, child)
  | otherwise =
    case visitedTwice of
      Just cv -> (not alreadySeen, Just cv, child)
      Nothing ->
        if alreadySeen
          then (True, Just child, child)
          else (True, Nothing, child)
  where
    alreadySeen = child `S.member` visited

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
