module Common.Debugging where

import           Common.Geometry (Point, renderVectorMap)
import qualified Data.Map        as M
import           Debug.Trace     (traceIO)
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
