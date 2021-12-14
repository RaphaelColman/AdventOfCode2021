module Common.Debugging where

import           Common.Geometry (Point, renderVectorMap)
import qualified Data.Map        as M
import           Debug.Trace     (traceIO, traceShow)
import           GHC.IO.Unsafe   (unsafePerformIO)

{-# NOINLINE traceLns #-}
traceLns :: String -> a -> a
traceLns string expr =
  unsafePerformIO $ do
    putStrLn string
    return expr

{-# NOINLINE traceVectorMap #-}
traceVectorMap :: M.Map Point Char -> a -> a
traceVectorMap vm = traceLns (renderVectorMap vm)

traceShowIf :: (Show a) => Bool -> a -> b -> b
traceShowIf pred showable rest =
  if pred
    then traceShow showable rest
    else rest
