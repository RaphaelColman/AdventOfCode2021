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

data AoCSolution a b =
  MkAoCSolution
    { _parser   :: Parser a
    , _solution :: a -> b
    }

type GetPuzzleInput = Integer -> ExceptT ConfigError IO String

printSolutions' ::
     (Show b) => Integer -> GetPuzzleInput -> AoCSolution a b -> IO ()
printSolutions' day puzzleInputFun (MkAoCSolution parser part1) = do
  result <-
    runExceptT $ do
      input <- puzzleInputFun day
      let parsed = parseString parser mempty input
      lift $ do
        putStrLn "Solution:"
        print $ part1 <$> parsed
  print result

printSolutions :: (Show b) => Integer -> AoCSolution a b -> IO ()
printSolutions = flip printSolutions' getPuzzleInput

printTestSolutions :: (Show b) => Integer -> AoCSolution a b -> IO ()
printTestSolutions = flip printSolutions' getTestPuzzleInput
