{-# LANGUAGE DataKinds       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Solutions.Day23
    ( aoc23
    ) where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.Debugging    ()
import           Common.Geometry     (Point, renderVectorMap)
import           Common.GraphUtils   (WGraphNode (neighbours), dijkstra)
import           Common.ListUtils    (flexibleRange, singleton, window3)
import           Common.MapUtils     (minimumValue)
import qualified Common.SetUtils     as SU
import           Control.Applicative ((<|>))
import           Control.Lens        (makeLenses, over)
import           Data.Char           (isLetter)
import           Data.Foldable       (find, minimumBy, traverse_)
import           Data.Function       (on)
import qualified Data.Map            as M
import           Data.Maybe          (isJust, isNothing, mapMaybe)
import qualified Data.PSQueue        as PQ
import qualified Data.Set            as S
import           Debug.Trace         (traceShow)
import           Linear              (Metric (dot), V2 (V2))
import           Text.Read           (Lexeme (Char))
import           Text.Trifecta       (CharParsing (anyChar, char), Parser,
                                      Parsing (skipMany), TokenParsing (token),
                                      charLiteral, count, integer, letter,
                                      manyTill, upper)

data BurrowSpace
  = CorridorSpace
      { _column :: Integer
      }
  | Room
      { _roomType :: AmType
      , _depth    :: Integer
      }
  deriving (Eq, Ord, Show)

data AmType = A | B | C | D deriving (Enum, Eq, Ord, Show)

data Amphipod
  = MkApod
      { _position :: BurrowSpace
      , _amType   :: AmType
      }
  deriving (Eq, Ord, Show)

type BurrowState = S.Set Amphipod

data Move
  = MkMove
      { _state :: BurrowState
      , _cost  :: Integer
      }
  deriving (Eq, Ord, Show)

makeLenses ''BurrowSpace
makeLenses ''Amphipod

aoc23 :: IO ()
aoc23 = do
  printSolutions 23 $ MkAoCSolution parseInput part1
  printSolutions 23 $ MkAoCSolution parseInput part2
  where
    debugLn (MkMove state cost) = do
      putStrLn $ renderVectorMap $ renderBurrowState state
      print cost

parseInput :: Parser BurrowState
parseInput = do
  podTypes <- count 8 untilLetter
  pure $ S.fromList $ zipWith MkApod rooms podTypes
  where
    untilLetter :: Parser AmType
    untilLetter = do
      c <- anyChar
      case c of
        'A' -> pure A
        'B' -> pure B
        'C' -> pure C
        'D' -> pure D
        _   -> untilLetter
    rooms = zipWith Room (cycle [A, B, C, D]) [1, 1, 1, 1, 2, 2, 2, 2]

part1 :: BurrowState -> Maybe Integer
part1 = findBestPath

part2 :: BurrowState -> Maybe Integer
part2 = findBestPath . unfoldApods

unfoldApods :: BurrowState -> BurrowState
unfoldApods bs = S.unions [depth1, extraApods, moveTo4 `S.map` depth2]
  where extraApods = S.fromList $ zipWith MkApod middleRooms [D,D,C,B,B,A,A,C]
        middleRooms = zipWith Room [A,A,B,B,C,C,D,D] $ cycle [2,3]
        (depth1, depth2) = S.partition (\(MkApod (Room _ depth) _) -> depth == 1) bs
        moveTo4 = over (position . depth) (const 4)

movementCost :: AmType -> Integer
movementCost aType =
  case aType of
    A -> 1
    B -> 10
    C -> 100
    D -> 1000

burrowComplete :: BurrowState -> Bool
burrowComplete bs = all (`isFinished` bs) bs

isFinished :: Amphipod -> BurrowState -> Bool
isFinished (MkApod position amType) bs =
  case position of
    CorridorSpace n -> False
    Room roomType roomDepth ->
      roomType == amType &&
      (let otherRoomOccupants = S.filter (inRoomType roomType) bs
        in all ((== roomType) . _amType) otherRoomOccupants)
      where inRoomType t (MkApod pos _) =
              case pos of
                CorridorSpace n -> False
                Room at _       -> at == t

occupied :: BurrowSpace -> BurrowState -> Bool
occupied bSpace bState = isJust $ getOccupant bSpace bState

getOccupant :: BurrowSpace -> BurrowState -> Maybe Amphipod
getOccupant bSpace = find (\(MkApod space aType) -> space == bSpace)

isOutsideRoom :: BurrowSpace -> Bool
isOutsideRoom (CorridorSpace i) = i `elem` [2, 4, 6, 8]
isOutsideRoom (Room _ _)        = False

destinationRoomSpace :: Amphipod -> BurrowState -> Maybe BurrowSpace
destinationRoomSpace (MkApod pos aType) bs
  | any (\(MkApod _ destType) -> destType /= aType) destinationOccupants =
    Nothing --Can't go this room because it has amphipods of the wrong type
  | otherwise =
    Just $ Room aType (roomDepth - toInteger (length destinationOccupants))
  | otherwise = error "More than two occupants for only two rooms"
  where
    destinationOccupants =
      mapMaybe ((`getOccupant` bs) . Room aType) [1 .. roomDepth]
    roomDepth = toInteger $ length $ S.filter ((== aType) . _amType) bs

nextMoves :: BurrowState -> S.Set Move
nextMoves bs = ($!) S.union toRoomMoves allCorridorMoves
  where
    moveable = S.filter (not . flip isFinished bs) bs
    toRoomMoves = SU.mapMaybe (`moveStraightToRoom` bs) moveable
    allCorridorMoves = S.fromList $ concatMap (`corridorMoves` bs) moveable

moveStraightToRoom :: Amphipod -> BurrowState -> Maybe Move
moveStraightToRoom aPod@(MkApod pos aType) bs = do
  ds <- destinationRoomSpace aPod bs
  let path = tail $ route pos ds
  let pathIsClear = not $ any (`occupied` bs) path
  newState <-
    if pathIsClear
      then pure $ S.insert (MkApod ds aType) $ S.filter (/= aPod) bs --Move the Apod to its destination
      else Nothing
  let cost = toInteger (length path) * movementCost aType
  pure $ MkMove newState cost

corridorMoves :: Amphipod -> BurrowState -> [Move]
corridorMoves aPod@(MkApod pos@(Room rType rNum) aType) bs =
  mapMaybe go $ filter (not . isOutsideRoom) $ map CorridorSpace [0 .. 10]
  where
    go cSpace =
      let path = tail $ route pos cSpace
          pathIsClear = not $ any (`occupied` bs) path
          cost = toInteger (length path) * movementCost aType
          newState = S.insert (MkApod cSpace aType) $ S.filter (/= aPod) bs
       in if pathIsClear
            then Just (MkMove newState cost)
            else Nothing
corridorMoves _ _ = []

newtype WGraphNodeBS
  = WGraphNodeBS { _bs :: BurrowState }
  deriving (Eq, Ord)

instance WGraphNode WGraphNodeBS where
  neighbours (WGraphNodeBS bs) = moves
    where moves = S.map (\(MkMove move cost) -> (WGraphNodeBS move, cost)) $ nextMoves bs

findBestPath :: BurrowState -> Maybe Integer
findBestPath burrowState = dijkstra burrowState nextMovesMap burrowComplete
  where nextMovesMap = M.fromList .
              map (\(MkMove state cost) -> (state, cost)) .
              S.toList .
              nextMoves

--Route from start node to end node (including start and end nodes)
route :: BurrowSpace -> BurrowSpace -> [BurrowSpace]
route (Room aType rNum) (CorridorSpace cNum) = roomStep ++ corridorSteps
  where
    corridorSteps =
      map CorridorSpace $ flexibleRange (aTypeToCorridorNum aType) cNum
    roomStep = map (Room aType) $ flexibleRange rNum 1
route (CorridorSpace cNum) (Room aType rNum) = corridorSteps ++ roomStep
  where
    corridorSteps =
      map CorridorSpace $ flexibleRange cNum (aTypeToCorridorNum aType)
    roomStep = map (Room aType) [1 .. rNum]
route r1@(Room aType1 rNum1) r2@(Room aType2 rNum2) =
  tail (route r1 nearestCorridorSpace) ++ route nearestCorridorSpace r2
  where
    nearestCorridorSpace = CorridorSpace $ aTypeToCorridorNum aType1
route (CorridorSpace _) (CorridorSpace _) =
  error "Should not be moving from one corridor space to another"

aTypeToCorridorNum :: AmType -> Integer
aTypeToCorridorNum aType =
  case aType of
    A -> 2
    B -> 4
    C -> 6
    D -> 8

--Utility functions for rendering the burrow to make it easier to debug
renderBurrow :: M.Map Point Char
renderBurrow = M.union corridorAndRooms entireGrid
  where
    corridor = map (`V2` 1) [1 .. 11]
    rooms1 = map (`V2` 2) [3, 5, 7, 9]
    rooms2 = map (`V2` 3) [3, 5, 7, 9]
    corridorAndRooms =
      M.fromList $ zip (corridor ++ rooms1 ++ rooms2) $ repeat '.'
    entireGrid =
      M.fromList $ zip [V2 x y | x <- [0 .. 12], y <- [0 .. 4]] $ repeat '#'

renderBurrowState :: BurrowState -> M.Map Point Char
renderBurrowState bs = M.union allApods renderBurrow
  where
    allApods =
      M.fromList $
      S.toList $ --Use S.fromKeySet with a function?
      S.map
        (\(MkApod space aType) -> (spaceToPoint space, amTypeToChar aType))
        bs

spaceToPoint :: BurrowSpace -> Point
spaceToPoint (CorridorSpace i) = V2 (fromInteger i + 1) 1
spaceToPoint (Room amType i) = V2 x $ fromInteger (i + 1)
  where
    x = fromInteger $ aTypeToX amType

aTypeToX :: AmType -> Integer
aTypeToX aType =
  case aType of
    A -> 3
    B -> 5
    C -> 7
    D -> 9

amTypeToChar :: AmType -> Char
amTypeToChar aType =
  case aType of
    A -> 'A'
    B -> 'B'
    C -> 'C'
    D -> 'D'

showBurrowState :: BurrowState -> IO ()
showBurrowState = putStrLn . renderVectorMap . renderBurrowState

showMove :: Move -> IO ()
showMove (MkMove state cost) = do
  showBurrowState state
  print cost

showMoves :: S.Set Move -> IO ()
showMoves moves = do
  traverse_ showMove moves
