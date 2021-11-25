module AoC1.ReportRepair where

import           Control.Monad.Trans.Except (except, runExceptT)
import           Data.Text                  (unpack)
import qualified Data.Text                  as T
import           Text.Trifecta              (TokenParsing (token), integer,
                                             some)
import           Text.Trifecta.Parser
import           Web.AoCUtils               (getPuzzleInput, getPuzzleInputE)

type Expenses = [Integer]

aoc1 :: IO ()
aoc1 = do
  result <-
    runExceptT $ do
      input <- getPuzzleInputE 1
      pure $ parseString parseExpenses mempty input
  print result

parseExpenses :: Parser Expenses
parseExpenses = do
  some $ token integer
