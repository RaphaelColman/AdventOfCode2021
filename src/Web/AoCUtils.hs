{-# LANGUAGE ScopedTypeVariables #-}

module Web.AoCUtils
  ( getPuzzleInput
  , getPuzzleInputE
  ) where

import           Advent
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

getPuzzleInput :: Integer -> IO (Either ConfigError String)
getPuzzleInput day = do
  loadFile defaultConfig
  sessionKey <- readEnv "SESSION_KEY"
  year <- readEnv "AOC_YEAR"
  let mOpts = liftA2 mkOpts sessionKey year
  case mOpts of
    Left error -> pure $ Left error
    Right opts -> (fmap . fmap) T.unpack (getPuzzleInput' day opts)

getPuzzleInputE :: Integer -> ExceptT ConfigError IO String
getPuzzleInputE x = ExceptT $ getPuzzleInput 1

mkOpts :: String -> String -> AoCOpts
mkOpts sessionKey year =
  AoCOpts sessionKey (read year :: Integer) (Just "res") False 3000000

getPuzzleInput' :: Integer -> AoCOpts -> IO (Either ConfigError T.Text)
getPuzzleInput' day opts = do
  result <- runAoC opts $ AoCInput $ mkDay_ day
  pure $
    case result of
      Left error -> Left $ MkAoCError error
      Right str  -> Right str

getPuzzleInput'' :: Integer -> AoCOpts -> ExceptT ConfigError IO T.Text
getPuzzleInput'' day opts = do
  withExceptT MkAoCError $ ExceptT $ runAoC opts $ AoCInput $ mkDay_ day

readEnv :: String -> IO (Either ConfigError String)
readEnv envVar = do
  loadFile defaultConfig
  env <- getEnv envVar
  pure $
    case env of
      Nothing  -> Left $ MkGenericConfigError $ "Failed to get " ++ envVar
      Just str -> Right str
