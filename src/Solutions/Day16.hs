{-# LANGUAGE DataKinds #-}

module Solutions.Day16
  ( aoc16
  ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.BinaryUtils  (fromHex, pad0, toBinaryString, toDecimal)
import           Common.ListUtils    (singleton)
import           Control.Monad       (guard)
import           Data.Finite         (Finite, finite)
import qualified Data.Map            as M
import           Text.Trifecta       (CharParsing (char), Parser, Result,
                                      TokenParsing (token), choice, count,
                                      digit, foldResult, hexDigit, many,
                                      parseString, some)

aoc16 :: IO ()
aoc16 = do
  printSolutions 16 $ MkAoCSolution parseInput part1
  printSolutions 16 $ MkAoCSolution parseInput part2

parseInput :: Parser String
parseInput = do
  binaryNumbers <- some $ hexLookup <$> hexDigit
  pure $ concat binaryNumbers

part1 :: String -> Result Integer
part1 input = addVersionNumbers <$> result
  where
    result = parseString parsePacket mempty input

part2 :: String -> Result Integer
part2 input = resolvePacket <$> result
  where
    result = parseString parsePacket mempty input

data Packet
  = PacketLiteral
      { version :: Integer
      , value   :: Integer
      }
  | PacketOperator
      { version :: Integer
      , typeId  :: TypeId
      , packets :: [Packet]
      }
  deriving (Eq, Show)

type TypeId = Finite 8

addVersionNumbers :: Packet -> Integer
addVersionNumbers = go 0
  where
    go accum (PacketLiteral version _) = accum + version
    go accum (PacketOperator version _ packets) = version + accum + packetVals
      where
        packetVals = foldr (flip go) 0 packets

resolvePacket :: Packet -> Integer
resolvePacket (PacketLiteral version value) = value
resolvePacket pk@(PacketOperator version typeId packets) =
  case typeId of
    0 -> multiPacketOp sum pk
    1 -> multiPacketOp product pk
    2 -> multiPacketOp minimum pk
    3 -> multiPacketOp maximum pk
    5 -> twoPacketOp (>) pk
    6 -> twoPacketOp (<) pk
    7 -> twoPacketOp (==) pk
    _ -> error ("unexpected operation: " ++ show typeId)

twoPacketOp :: (Integer -> Integer -> Bool) -> Packet -> Integer
twoPacketOp fun (PacketOperator version typeId [packet1, packet2]) =
  (toInteger . fromEnum) $ resolvePacket packet1 `fun` resolvePacket packet2
twoPacketOp _ pk = error ("unexpected twoPacketOp on packet: " ++ show pk)

multiPacketOp :: ([Integer] -> Integer) -> Packet -> Integer
multiPacketOp fun packet =
  case packet of
    PacketLiteral _ value      -> value
    PacketOperator _ _ packets -> fun $ map resolvePacket packets

parsePacket :: Parser Packet
parsePacket = do
  version <- toDecimal <$> count 3 digit
  typeId <- finite . toDecimal <$> count 3 digit
  if typeId == 4
    then parsePacketLiteral version
    else parsePacketOperator version typeId

parsePacketOperator :: Integer -> TypeId -> Parser Packet
parsePacketOperator version typeId = do
  lengthTypeId <- digit >>= lengthLookup
  subPackets <-
    case lengthTypeId of
      NumBits n    -> parseSection n
      NumPackets n -> count (fromInteger n) parsePacket
  pure $ PacketOperator version typeId subPackets
  where
    lengthLookup l
      | l == '0' = NumBits . toDecimal <$> count 15 digit --This is correct - number representing length in bits of subpackets
      | l == '1' = NumPackets . toDecimal <$> count 11 digit --This is wrong - number represenging the number of subpackets
      | otherwise = fail "Unexpected non-binary digit"
    parseSection lengthSubpackets = do
      sectionToParse <- count (fromInteger lengthSubpackets) digit
      let result = parseString (some parsePacket) mempty sectionToParse
      foldResult
        (\errInfo -> fail ("Inner parser failed" ++ show errInfo))
        pure
        result

data LengthTypeID
  = NumBits Integer
  | NumPackets Integer
  deriving (Eq, Show)

parsePacketLiteral :: Integer -> Parser Packet
parsePacketLiteral version = do
  groups <- toDecimal <$> parseGroups
  pure $ PacketLiteral version groups

parseGroups :: Parser String
parseGroups = do
  first <- digit
  case first of
    '0' -> count 4 digit
    '1' -> do
      thisGroup <- count 4 digit
      (thisGroup ++) <$> parseGroups
    _ -> fail $ "Unexpected non-binary digit" ++ show first

hexLookup :: Char -> String
hexLookup = pad0 4 . toBinaryString . fromHex . singleton
