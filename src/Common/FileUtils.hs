{-# LANGUAGE OverloadedStrings #-}

module Common.FileUtils where

import           Data.Text               (unpack)
import           Data.Text.Format
import           Data.Text.Internal.Lazy (showStructure)
import           Data.Text.Lazy          (toChunks, toStrict)
import           System.Directory
import           System.IO

readFileToString :: String -> IO String
readFileToString filePath = do
  handle <- openFile filePath ReadMode
  hGetContents handle

getTestInputFile :: String -> Int -> IO String
getTestInputFile year puzzleNumber = do
  workingDirectory <- getCurrentDirectory
  let path =
        format
          "{}/res/test_inputs/{}/{}.txt"
          [workingDirectory, year, show puzzleNumber]
  readFileToString $ unpack $ toStrict path
