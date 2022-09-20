{-# LANGUAGE ScopedTypeVariables #-}

module Web.AoCUtils
  ( getPuzzleInput
  , getTestPuzzleInput
  , ConfigError(MkGenericConfigError)
  ) where

import Advent
    ( runAoC, mkDay_, AoC(AoCInput), AoCError, AoCOpts(AoCOpts) )
import           Common.FileUtils           (getTestInputFile)
import           Configuration.Dotenv       (defaultConfig, loadFile)
import           Control.Monad.Trans.Class  (MonadTrans (lift))
import           Control.Monad.Trans.Except (Except, ExceptT (ExceptT), except,
                                             runExceptT, withExceptT)
import qualified Data.Text                  as T
import           GHC.Base                   (Applicative (liftA2))
import           GHC.IO                     (liftIO)
import           GHC.TopHandler             (runIO)
import           System.Environment.Blank   (getEnv)

data ConfigError
  = MkAoCError AoCError
  | MkGenericConfigError String
  deriving (Show)

getPuzzleInput :: Integer -> ExceptT ConfigError IO String
getPuzzleInput day = do
  sessionKey <- readEnv "SESSION_KEY"
  year <- readEnv "AOC_YEAR"
  let mOpts = mkOpts sessionKey year
  input <- getPuzzleInput' day mOpts
  pure $ T.unpack input

getTestPuzzleInput :: Integer -> ExceptT ConfigError IO String
getTestPuzzleInput day = do
  year <- readEnv "AOC_YEAR"
  ExceptT $ do
    input <- getTestInputFile year (fromInteger day)
    pure $ Right input

mkOpts :: String -> String -> AoCOpts
mkOpts sessionKey year =
  AoCOpts sessionKey (read year :: Integer) (Just "res") False 3000000

getPuzzleInput' :: Integer -> AoCOpts -> ExceptT ConfigError IO T.Text
getPuzzleInput' day opts = do
  withExceptT MkAoCError $ ExceptT $ runAoC opts $ AoCInput $ mkDay_ day

readEnv :: String -> ExceptT ConfigError IO String
readEnv envVar =
  ExceptT $ do
    loadFile defaultConfig
    env <- getEnv envVar
    pure $
      case env of
        Nothing  -> Left $ MkGenericConfigError $ "Failed to get " ++ envVar
        Just str -> Right str
