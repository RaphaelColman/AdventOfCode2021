{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
module Blog.UploadBlog
    ( doUpload
    ) where

import           Control.Lens                 ((&), (.~), (?~))
import           Control.Monad.Cont           (MonadTrans (lift))
import           Control.Monad.Trans.Maybe    (MaybeT (runMaybeT))
import qualified Data.HashMap.Strict          as HashMap
import           Data.Text                    (pack)
import qualified Data.Text                    as Text
import           Data.Time                    (getCurrentTime)
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Data.Time.Clock.System       (SystemTime (MkSystemTime),
                                               getSystemTime)
import           Data.UUID                    (UUID, toString, toText)
import           Data.UUID.V4                 (nextRandom)
import           Network.AWS                  (Credentials (Discover),
                                               HasEnv (envLogger),
                                               LogLevel (Debug),
                                               Region (London), newEnv,
                                               newLogger, runAWS, runResourceT,
                                               send, within)
import           Network.AWS.Auth             (fromProfile)
import           Network.AWS.DynamoDB         (AttributeValue, attributeValue,
                                               avS)
import           Network.AWS.DynamoDB.PutItem (PutItem, PutItemResponse, piItem,
                                               putItem)
import           Network.AWS.DynamoDB.Types   (avN)
import           Network.AWS.S3               (Region (London))
import           ParseReadme                  (parseReadme, Section(MkSection))
import           System.IO                    (stdout)
import Data.Foldable (traverse_)


--I'm missing authorId, so the index does not populate
sectionToPutItem :: Section -> IO PutItem
sectionToPutItem (MkSection title body) = do
  aUuid <- toText <$> nextRandom
  timestamp <- round . (*1000) <$> getPOSIXTime
  let hm = HashMap.fromList
            [
                ("id", attributeValue & avS ?~ aUuid)
              , ("title", attributeValue & avS ?~ pack title)
              , ("content", attributeValue & avS ?~ pack body)
              , ("timestamp", attributeValue & avN ?~ pack (show timestamp))
            ]
  pure $ putItem "BaffledRafflesBlogEntries" & piItem .~ hm


uploadBlogEntry :: PutItem -> IO PutItemResponse
uploadBlogEntry putItem = do
    -- A new Logger to replace the default noop logger is created, with the logger
    -- set to print debug information and errors to stdout:
    lgr  <- newLogger Debug stdout
    -- To specify configuration preferences, newEnv is used to create a new
    -- configuration environment. The Credentials parameter is used to specify
    -- mechanism for supplying or retrieving AuthN/AuthZ information.
    -- In this case Discover will cause the library to try a number of options such
    -- as default environment variables, or an instance's IAM Profile and identity document:
    env  <- newEnv Discover
    runResourceT $ runAWS (env & envLogger .~ lgr) $
        within London $
            send putItem

uploadBlogEntries :: MaybeT IO [PutItemResponse]
uploadBlogEntries = do
  chapters <- parseReadme
  putItems <- lift $ traverse sectionToPutItem chapters
  lift $ traverse uploadBlogEntry putItems

doUpload :: IO ()
doUpload = do
  runMaybeT uploadBlogEntries
  print "done"
