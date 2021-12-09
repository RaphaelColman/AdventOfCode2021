module Solutions.Day8
  ( aoc8
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Control.Monad       (msum)
import           Data.List           (find, permutations)
import qualified Data.Map            as M
import           Data.Maybe          (isJust)
import qualified Data.Set            as S
import           Text.Trifecta       (CharParsing (char, string), Parser,
                                      TokenParsing (token), count, letter, some)

aoc8 :: IO ()
aoc8 = do
  printSolutions 8 $ MkAoCSolution parseInput part1
  printSolutions 8 $ MkAoCSolution parseInput part2

type Combo = S.Set Char

type Entry = ([Combo], [Combo])

type Mapping = M.Map Char Char

parseInput :: Parser [Entry]
parseInput = do
  token $
    some $ do
      signals <- count 10 $ token $ some letter
      string "| "
      output <- count 4 $ token $ some letter
      pure (S.fromList <$> signals, S.fromList <$> output)

part1 :: [Entry] -> Int
part1 entries =
  length $ filter (\e -> length e `elem` [2, 3, 4, 7]) $ concatMap snd entries

part2 :: [Entry] -> Maybe Integer
part2 entries = sum <$> traverse tryMappings entries

tryMappings :: Entry -> Maybe Integer
tryMappings (wires, output) = do
  goodMapping <- find (isJust . (`decodeCombos` wires)) allMappings
  decoded <- decodeCombos goodMapping output
  pure $ read (concatMap show decoded)

decodeCombos :: Mapping -> [Combo] -> Maybe [Integer]
decodeCombos mapping = traverse (decodeCombo mapping)
  where
    decodeCombo mapping' combo' =
      let mapped = S.map (mapping' M.!) combo'
       in M.lookup mapped knownCombos

allMappings :: [Mapping]
allMappings = map (toMapping "abcdefg") (permutations "abcdefg")
  where
    toMapping str1 str2 = M.fromList $ zip str1 str2

knownCombos :: M.Map Combo Integer
knownCombos = M.fromList $ zip combos [0 .. 9]
  where
    combos =
      map
        S.fromList
        [ "abcdef"
        , "bc"
        , "abdeg"
        , "abcdg"
        , "bcfg"
        , "acdfg"
        , "acdefg"
        , "abc"
        , "abcdefg"
        , "abcdfg"
        ]
