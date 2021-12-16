module Solutions.Day16 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Control.Monad       (guard)
import qualified Data.Map            as M
import           Text.Trifecta       (CharParsing (char), Parser,
                                      TokenParsing (token), choice, count,
                                      digit, foldResult, hexDigit, many,
                                      parseString, some, Result)

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
  where result = parseString parsePacket mempty input

data Packet
  = PacketLiteral
      { version :: Integer
      , value   :: Integer
      }
  | PacketOperator
      { version :: Integer
      , typeId  :: Integer
      , packets :: [Packet]
      }
  deriving (Eq, Show)

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
    0 -> sumOp pk
    1 -> prodOp pk
    2 -> minOp pk
    3 -> maxOp pk
    5 -> greaterOp pk
    6 -> lessOp pk
    7 -> equalOp pk
    _ -> error ("unexpected operation: " ++ show typeId)

sumOp :: Packet -> Integer
sumOp = multiPacketOp sum

prodOp :: Packet -> Integer
prodOp = multiPacketOp product

minOp :: Packet -> Integer
minOp = multiPacketOp minimum

maxOp :: Packet -> Integer
maxOp = multiPacketOp maximum

greaterOp :: Packet -> Integer
greaterOp (PacketOperator version typeId [packet1, packet2]) =
  if resolvePacket packet1 > resolvePacket packet2
    then 1
    else 0
greaterOp pk = error ("unexpected greaterOp on packet: " ++ show pk)

lessOp :: Packet -> Integer
lessOp (PacketOperator version typeId [packet1, packet2]) =
  if resolvePacket packet1 < resolvePacket packet2
    then 1
    else 0
lessOp pk = error ("unexpected lessOp on packet: " ++ show pk)

equalOp :: Packet -> Integer
equalOp (PacketOperator version typeId [packet1, packet2]) =
  if resolvePacket packet1 == resolvePacket packet2
    then 1
    else 0
equalOp pk = error ("unexpected equalOp on packet: " ++ show pk)

multiPacketOp :: ([Integer] -> Integer) -> Packet -> Integer
multiPacketOp fun packet =
  case packet of
    PacketLiteral _ value      -> value
    PacketOperator _ _ packets -> fun $ map resolvePacket packets

parsePacket :: Parser Packet
parsePacket = do
  version <- toDecimal <$> count 3 digit
  typeId <- toDecimal <$> count 3 digit
  if typeId == 4
    then parsePacketLiteral version
    else parsePacketOperator version typeId

parsePacketOperator :: Integer -> Integer -> Parser Packet
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
      let asPackets =
            foldResult
              (\errInfo -> error ("Inner parser failed" ++ show errInfo))
              id
              result
      pure asPackets

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
  midGroups <- many parseMidGroup
  lastGroup <- parseLastGroup
  pure $ concat midGroups ++ lastGroup

parseLastGroup :: Parser String
parseLastGroup = do
  char '0'
  count 4 digit

parseMidGroup :: Parser String
parseMidGroup = do
  char '1'
  count 4 digit

hexLookup :: Char -> String
hexLookup c = mp M.! c
  where
    hexDigits = "0123456789ABCDEF"
    mp =
      M.fromList $!
      zip
        hexDigits
        [ "0000"
        , "0001"
        , "0010"
        , "0011"
        , "0100"
        , "0101"
        , "0110"
        , "0111"
        , "1000"
        , "1001"
        , "1010"
        , "1011"
        , "1100"
        , "1101"
        , "1110"
        , "1111"
        ]

toDecimal :: String -> Integer
toDecimal = sum . (zipWith (*) [2 ^ n | n <- [0,1 ..]]) . reverse . asIntList
  where
    asIntList :: String -> [Integer]
    asIntList = map (toInteger . \c -> read [c])
