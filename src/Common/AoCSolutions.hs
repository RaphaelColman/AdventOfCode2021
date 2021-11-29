module Common.AoCSolutions
  ( printSolutions
  , printTestSolutions
  , AoCSolution(MkAoCSolution)
  ) where

import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import           Text.Trifecta              (Parser)
import           Text.Trifecta.Parser       (parseString)
import           Web.AoCUtils               (ConfigError, getPuzzleInput,
                                             getTestPuzzleInput)

data AoCSolution a b c =
  MkAoCSolution
    { _parser :: Parser a
    , _part1  :: a -> b
    , _part2  :: a -> c
    }

type GetPuzzleInput = Integer -> ExceptT ConfigError IO String

printSolutions' ::
     (Show b, Show c) => Integer -> GetPuzzleInput -> AoCSolution a b c -> IO ()
printSolutions' day puzzleInputFun (MkAoCSolution parser part1 part2) = do
  result <-
    runExceptT $ do
      input <- puzzleInputFun day
      let parsed = parseString parser mempty input
      lift $ do
        putStrLn "Part 1:"
        print $ part1 <$> parsed
        putStrLn "Part 2:"
        print $ part2 <$> parsed
  print result

printSolutions :: (Show b, Show c) => Integer -> AoCSolution a b c -> IO ()
printSolutions = flip printSolutions' getPuzzleInput

printTestSolutions :: (Show b, Show c) => Integer -> AoCSolution a b c -> IO ()
printTestSolutions = flip printSolutions' getTestPuzzleInput
