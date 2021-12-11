module Common.MapUtils where

import qualified Data.Map as M

mapIf :: (a -> Bool) -> (a -> a) -> M.Map k a -> M.Map k a
mapIf condition f =
  M.map
    (\a ->
       if condition a
         then f a
         else a)
