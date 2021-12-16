module Common.BinaryUtils where

import Numeric.Lens ( binary, hex )
import Control.Lens ((^?), (^?!), (^.), re)

toDecimal :: String -> Integer
toDecimal = sum . (zipWith (*) [2 ^ n | n <- [0,1 ..]]) . reverse . asIntList
  where
    asIntList :: String -> [Integer]
    asIntList = map (toInteger . \c -> read [c])

fromHex :: Integral a => String -> a
fromHex str = str ^?! hex

toBinaryString :: Integral s => s -> String
toBinaryString x = x ^. re binary

pad0 :: Int -> String -> String
pad0 targetLen str = replicate times '0' ++ str
  where times = targetLen - length str