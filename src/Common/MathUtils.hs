module Common.MathUtils where

triangleX :: (Integral a) => a -> a
triangleX x = x * (x + 1) `div` 2
