{-# LANGUAGE ScopedTypeVariables #-}

module Solutions.Day21
  ( aoc21
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (freqs, window3, windowN)
import           Common.MapUtils     (partitionKeys)
import           Control.Lens        ((&))
import           Data.Bifunctor      (first)
import           Data.Either         (fromLeft, isLeft, partitionEithers)
import           Data.List           (partition)
import           Data.List.Split     (chunksOf)
import qualified Data.Map            as M
import           Text.Trifecta       (CharParsing (string), Parser, integer)

aoc21 :: IO ()
aoc21 = do
  printSolutions 21 $ MkAoCSolution parseInput part1
  printSolutions 21 $ MkAoCSolution parseInput part2

type Players = (Integer, Integer)

data Player =
  MkPlayer
    { _value :: Integer
    , _score :: Integer
    }
  deriving (Eq, Show, Ord)

data Game =
  MkGame
    { _player1  :: Player
    , _player2  :: Player
    , _rolls    :: [Integer]
    , _numRolls :: Integer
    }
  deriving (Eq)

instance Show Game where
  show (MkGame p1 p2 rolls numRolls) =
    show ((show p1, show p2, show numRolls), show (take 10 rolls))

initGame :: Players -> Game
initGame (player1, player2) =
  MkGame (MkPlayer player1 0) (MkPlayer player2 0) rolls 0
  where
    rolls = map sum $ chunksOf 3 $ cycle [1 .. 100]

parseInput :: Parser Players
parseInput = do
  player1 <- string "Player 1 starting position: " >> integer
  player2 <- string "Player 2 starting position: " >> integer
  pure (player1, player2)

part1 :: Players -> Integer
part1 = play . initGame

part2 :: Players -> Integer
part2 = playDiracGame . initDiracGame

play :: Game -> Integer
play game =
  case loser game of
    Just (losingScore, numRolls) -> losingScore * numRolls
    Nothing                      -> step game & play

step :: Game -> Game
step (MkGame (MkPlayer value score) player2 (thisRoll:rest) numRolls) =
  let newValue = incrementValue value thisRoll
      newScore = score + newValue
   in MkGame player2 (MkPlayer newValue newScore) rest (numRolls + 3)

loser :: Game -> Maybe (Integer, Integer)
loser (MkGame (MkPlayer _ score1) (MkPlayer _ score2) _ numRolls)
  | score1 >= 1000 = Just (score2, numRolls)
  | score2 >= 1000 = Just (score1, numRolls)
  | otherwise = Nothing

incrementValue :: Integer -> Integer -> Integer
incrementValue value amount =
  let total = (value + amount) `mod` 10
   in if total == 0
        then 10
        else total

diracDiceRolls :: M.Map Integer Integer
diracDiceRolls = freqs combos
  where
    combos = [sum [x, y, z] | x <- [1 .. 3], y <- [1 .. 3], z <- [1 .. 3]]

data Universe =
  MkU
    { _dPlayer1 :: Player
    , _dPlayer2 :: Player
    }
  deriving (Eq, Show, Ord)

data DiracGame =
  MkDG
    { _universes   :: Universes
    , _firstPlayer :: Bool
    , _player1Wins :: Integer
    , _player2Wins :: Integer
    }
  deriving (Eq, Show)

type Universes = M.Map Universe Integer

data Winner
  = PLAYER1
  | PLAYER2
  deriving (Eq, Show, Ord)

playDiracGame :: DiracGame -> Integer
playDiracGame game@(MkDG universes _ p1W p2W)
  | null universes = max p1W p2W
  | otherwise = diracTurn game & playDiracGame

diracTurn :: DiracGame -> DiracGame
diracTurn (MkDG universes firstPlayer player1Wins player2Wins) =
  MkDG remaining (not firstPlayer) newP1Wins newP2Wins
  where
    newUniverses = M.foldrWithKey go M.empty universes
    go :: Universe -> Integer -> Universes -> Universes
    go universe count universes =
      let newUniverses = splitUniverse firstPlayer universe count
       in M.unionWith (+) newUniverses universes
    (finished, remaining) = partitionKeys partitionFinished (+) newUniverses
    newP1Wins = M.findWithDefault 0 PLAYER1 finished + player1Wins
    newP2Wins = M.findWithDefault 0 PLAYER2 finished + player2Wins

partitionFinished :: Universe -> Either Winner Universe
partitionFinished universe@(MkU (MkPlayer _ score1) (MkPlayer _ score2))
  | score1 >= 21 = Left PLAYER1
  | score2 >= 21 = Left PLAYER2
  | otherwise = Right universe

splitUniverse :: Bool -> Universe -> Integer -> Universes
splitUniverse isPlayer1 (MkU player1 player2) count =
  M.map (* count) $ M.mapKeys go diracDiceRolls
  where
    go value =
      if isPlayer1
        then MkU (newPlayer value player1) player2
        else MkU player1 (newPlayer value player2)

newPlayer :: Integer -> Player -> Player
newPlayer value player =
  let newValue = incrementValue (_value player) value
      newScore = _score player + newValue
   in MkPlayer newValue newScore

initDiracGame :: Players -> DiracGame
initDiracGame (p1, p2) = MkDG (M.fromList [(initialUniverse, 1)]) True 0 0
  where
    [player1, player2] = map (`MkPlayer` 0) [p1, p2]
    initialUniverse = MkU player1 player2
