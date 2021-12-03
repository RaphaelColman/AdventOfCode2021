module Common.EnumUtils where

enumNext :: (Enum a, Eq a, Bounded a) => a -> a
enumNext e
  | e == maxBound = minBound
  | otherwise = succ e

enumPrev :: (Enum a, Eq a, Bounded a) => a -> a
enumPrev e
  | e == minBound = maxBound
  | otherwise = pred e

stepEnum :: (Enum a, Eq a, Bounded a) => a -> Int -> a
stepEnum enum times = iterate enumNext enum !! times

prevStepEnum :: (Enum a, Eq a, Bounded a) => a -> Int -> a
prevStepEnum enum times = iterate enumPrev enum !! times
