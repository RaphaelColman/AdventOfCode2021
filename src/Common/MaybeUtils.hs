module Common.MaybeUtils where

loopMaybe :: (a -> Maybe a) -> (a -> Bool) -> a -> Maybe a
loopMaybe step shouldFinish state
  | shouldFinish state = pure state
  | otherwise = step state >>= loopMaybe step shouldFinish
