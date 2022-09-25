{-# LANGUAGE OverloadedStrings #-}
module Blog.UploadBlog where

import           Control.Lens                 ((&), (.~), (?~))
import qualified Data.HashMap.Strict          as HashMap
import           Data.Text
import qualified Data.Text                    as Text
import           Network.AWS
import           Network.AWS.DynamoDB         (attributeValue, avS)
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Types   (avN)
import           Network.AWS.S3
import           System.IO
import Network.AWS.Auth (fromProfile)

putItemTest :: IO PutItemResponse
putItemTest = do
    -- A new Logger to replace the default noop logger is created, with the logger
    -- set to print debug information and errors to stdout:
    lgr  <- newLogger Debug stdout

    -- To specify configuration preferences, newEnv is used to create a new
    -- configuration environment. The Credentials parameter is used to specify
    -- mechanism for supplying or retrieving AuthN/AuthZ information.
    -- In this case Discover will cause the library to try a number of options such
    -- as default environment variables, or an instance's IAM Profile and identity document:
    env  <- newEnv Discover

    -- The payload (and hash) for the S3 object is retrieved from a FilePath,
    -- either hashedFile or chunkedFile can be used, with the latter ensuring
    -- the contents of the file is enumerated exactly once, during send:
    let item = HashMap.fromList
                [
                  ("pk", attributeValue & avS ?~ "1234")
                , ("sk", attributeValue & avS ?~ "A blog entry")
                ]
    let pi = putItem "BaffledRafflesBlogEntries" & piItem .~ item

    -- We now run the AWS computation with the overriden logger, performing the
    -- PutObject request. envRegion or within can be used to set the
    -- remote AWS Region:
    runResourceT $ runAWS (env & envLogger .~ lgr) $
        within London $
            send pi

intToText :: Int -> Text
intToText = Text.pack . show
