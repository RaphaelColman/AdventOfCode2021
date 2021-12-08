module Common.AoCSolutions
  ( printSolutions
  , printTestSolutions
  , AoCSolution(MkAoCSolution)
  ) where

import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Except (ExceptT, except, runExceptT,
                                             withExceptT)
import           GHC.Base                   (Any)
import           Text.Trifecta              (ErrInfo, Parser, Result,
                                             foldResult)
import           Text.Trifecta.Parser       (parseString)
import           Web.AoCUtils               (ConfigError, getPuzzleInput,
                                             getTestPuzzleInput)

data AoCSolution a b =
  MkAoCSolution
    { _parser   :: Parser a
    , _solution :: a -> b
    }

type GetPuzzleInput = Integer -> ExceptT ConfigError IO String

data SolutionError
  = MkConfigError ConfigError
  | MKParseError ErrInfo
  deriving (Show)

printSolutions' ::
     (Show b) => Integer -> GetPuzzleInput -> AoCSolution a b -> IO ()
printSolutions' day puzzleInputFun (MkAoCSolution parser part1) = do
  result <-
    runExceptT $ do
      input <- withExceptT MkConfigError $ puzzleInputFun day
      parsed <- except $ resultToEither $ parseString parser mempty input
      lift $ do
        putStrLn "Solution:"
        print $ part1 parsed
  either print return result

printSolutions :: (Show b) => Integer -> AoCSolution a b -> IO ()
printSolutions = flip printSolutions' getPuzzleInput

printTestSolutions :: (Show b) => Integer -> AoCSolution a b -> IO ()
printTestSolutions = flip printSolutions' getTestPuzzleInput

resultToEither :: Result a -> Either SolutionError a
resultToEither = foldResult (Left . MKParseError) Right
