module Common.SetUtils where

import qualified Data.Set as S
import Data.Maybe (isJust, fromJust)

mapMaybe :: (Ord b) => (a -> Maybe b) -> S.Set a -> S.Set b
mapMaybe f = S.map fromJust . S.filter isJust . S.map f 