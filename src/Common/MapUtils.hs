module Common.MapUtils where

import           Data.Either   (partitionEithers)
import           Data.Foldable (minimumBy)
import           Data.Function (on)
import qualified Data.Map      as M

mapIf :: (a -> Bool) -> (a -> a) -> M.Map k a -> M.Map k a
mapIf condition f =
  M.map
    (\a ->
       if condition a
         then f a
         else a)

partitionKeys ::
     (Ord k, Ord b, Ord c)
  => (k -> Either b c)
  -> (a -> a -> a)
  -> M.Map k a
  -> (M.Map b a, M.Map c a)
partitionKeys f combining map' =
  (M.fromListWith combining lefts, M.fromListWith combining rights)
  where
    (lefts, rights) =
      partitionEithers $
      map
        (\(k, a) ->
           let e = f k
            in either (\x -> Left (x, a)) (\x -> Right (x, a)) e) $
      M.toList map'

minimumValue :: (Ord a) => M.Map k a -> (k, a)
minimumValue = minimumBy (compare `on` snd) . M.toList
