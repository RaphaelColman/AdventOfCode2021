module Solutions.Day21 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.ListUtils    (window3, windowN)
import           Control.Lens        ((&))
import           Data.List.Split (chunksOf)
import           Text.Trifecta       (CharParsing (string), Parser, integer)

aoc21 :: IO ()
aoc21 = do
  printSolutions 21 $ MkAoCSolution parseInput part1
  --printSolutions 21 $ MkAoCSolution parseInput part2

type Players = (Integer, Integer)

data Player =
  MkPlayer
    { _value :: Integer
    , _score :: Integer
    }
  deriving (Eq, Show)

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

--part1 :: String -> String
part1 players = play game
  where game = initGame players
        finGame = iterate step game !! 331

--part2 :: String -> String
part2 = undefined

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
incrementValue score amount =
  let total = (score + amount) `mod` 10
   in if total == 0
        then 10
        else total
