module Common.BinaryUtils where

toDecimal :: String -> Integer
toDecimal = sum . (zipWith (*) [2 ^ n | n <- [0,1 ..]]) . reverse . asIntList
  where
    asIntList :: String -> [Integer]
    asIntList = map (toInteger . \c -> read [c])
