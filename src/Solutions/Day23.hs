{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day23 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Common.Debugging
import           Common.Geometry     (Point)
import           Common.ListUtils    (flexibleRange, singleton, window3)
import           Control.Lens
import           Data.Foldable       (find)
import qualified Data.Map            as M
import           Data.Maybe          (isJust, isNothing, mapMaybe)
import qualified Data.Set            as S
import           Linear              (V2 (V2))
import           Text.Trifecta       (Parser)

data BurrowSpace
  = CorridorSpace Integer
  | Room AmType Integer
  deriving (Eq, Show, Ord)

data AmType
  = A
  | B
  | C
  | D
  deriving (Eq, Show, Ord, Enum)

type Burrow = M.Map BurrowSpace (S.Set BurrowSpace)

data Amphipod =
  MkApod
    { _position :: BurrowSpace
    , _amType   :: AmType
    }
  deriving (Eq, Show, Ord)

data MovingState
  = RoomToCorridor
  | CorridorToRoom
  deriving (Eq, Show, Enum, Ord)

data BurrowState =
  MkBS
    { _theBurrow :: Burrow
    , _aPods     :: [Amphipod]
    }
  deriving (Eq, Show, Ord)

makeLenses ''Amphipod

makeLenses ''BurrowState

aoc23 :: IO ()
aoc23 = do
  printSolutions 23 $ MkAoCSolution parseInput part1
  --printSolutions 23 $ MkAoCSolution parseInput part2

parseInput :: Parser Burrow
parseInput = pure burrow

--part1 :: String -> String
part1 burrow = traceVectorMap (renderBurrowState bs) finished
  where
    bs = initBurrowState
    finished = filter (`isFinished` bs) $ bs ^. aPods

part2 :: String -> String
part2 = undefined

initBurrowState :: BurrowState
initBurrowState = MkBS burrow amphipods
  where
    amphipods = zipWith MkApod rooms podTypes
    podTypes = [B, A, C, D, B, C, D, A]
    rooms = zipWith Room [A, A, B, B, C, C, D, D] $ cycle [1, 2]

isFinished :: Amphipod -> BurrowState -> Bool
isFinished (MkApod position amType) bs
  | position == Room amType 2 = True -- no need to move if you're in the deepest room of your type
  | position == Room amType 1 =
    case getOccupant (Room amType 2) bs of
      Just a  -> _amType a == amType --Finished if you're in room 1 and room 2 is occupied by an amphipod of the correct type
      Nothing -> False
  | otherwise = False

inCorridor :: Amphipod -> Bool
inCorridor (MkApod position _) =
  case position of
    CorridorSpace _ -> True
    Room at _       -> False

possibleAdjacentPositions :: Amphipod -> BurrowState -> S.Set BurrowSpace
possibleAdjacentPositions (MkApod position amType) bs =
  S.filter (\space -> isNothing (getOccupant space bs)) $
  _theBurrow bs M.! position

occupied :: BurrowSpace -> BurrowState -> Bool
occupied bSpace bState = isJust $ getOccupant bSpace bState

getOccupant :: BurrowSpace -> BurrowState -> Maybe Amphipod
getOccupant bSpace bState =
  find (\(MkApod space aType) -> space == bSpace) $ bState ^. aPods

isRoom :: BurrowSpace -> Bool
isRoom (CorridorSpace _) = False
isRoom (Room _ _)        = True

isOutsideRoom :: BurrowSpace -> Bool
isOutsideRoom (CorridorSpace i) = i `elem` [2, 4, 6, 8]
isOutsideRoom (Room _ _)        = False

destinationRoomSpace :: Amphipod -> BurrowState -> Maybe BurrowSpace
destinationRoomSpace (MkApod pos aType) bs
  | any (\(MkApod _ destType) -> destType /= aType) destinationOccupants =
    Nothing --Can't go this room because it has amphipods of the wrong type
  | null destinationOccupants = Just $ Room aType 2
  | length destinationOccupants == 1 = Just $ Room aType 1
  | length destinationOccupants == 2 = Nothing --This room is already complete
  | otherwise = error "More than two occupants for only two rooms"
  where
    destinationOccupants = mapMaybe ((`getOccupant` bs) . Room aType) [1, 2]

--Route from start node to end node (including start and end ndoes)
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
    roomStep = map (Room aType) $ [1 .. rNum]
route (Room aType1 rNum1) (Room aType2 rNum2) =
  roomStepOut ++ corridorSteps ++ roomStepIn
  where
    corridorSteps =
      map CorridorSpace $
      flexibleRange (aTypeToCorridorNum aType1) (aTypeToCorridorNum aType2)
    roomStepOut = map (Room aType1) $ flexibleRange rNum1 1
    roomStepIn = map (Room aType2) [1 .. rNum2]
route (CorridorSpace _) (CorridorSpace _) =
  error "Should not be moving from one corridor space to another"

aTypeToCorridorNum :: AmType -> Integer
aTypeToCorridorNum aType =
  case aType of
    A -> 2
    B -> 4
    C -> 6
    D -> 8

--Creating the burrow graph
burrow :: Burrow
burrow = M.union corridorSpaces rooms

corridorSpaces :: M.Map BurrowSpace (S.Set BurrowSpace)
corridorSpaces = M.unionsWith S.union [triples, roomJoins, edges]
  where
    triples =
      M.fromList $
      map (\(a, b, c) -> (b, S.fromList [a, c])) $ window3 allSpaces
    edges =
      M.fromList
        [ (CorridorSpace 0, S.fromList [CorridorSpace 1])
        , (CorridorSpace 10, S.fromList [CorridorSpace 9])
        ]
    allSpaces = map CorridorSpace [0 .. 10]
    roomJoins =
      M.fromList $
      zip (map CorridorSpace [2,4 .. 8]) $
      map (S.fromList . singleton . flip Room 1) [A .. D]

rooms :: M.Map BurrowSpace (S.Set BurrowSpace)
rooms =
  M.fromList
    [ (Room A 1, S.fromList [Room A 2, CorridorSpace 2])
    , (Room B 1, S.fromList [Room B 2, CorridorSpace 4])
    , (Room C 1, S.fromList [Room C 2, CorridorSpace 6])
    , (Room D 1, S.fromList [Room D 2, CorridorSpace 8])
    , (Room A 2, S.fromList [Room A 1])
    , (Room B 2, S.fromList [Room B 1])
    , (Room C 2, S.fromList [Room C 1])
    , (Room D 2, S.fromList [Room D 1])
    ]

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
      map (\(MkApod space aType) -> (spaceToPoint space, amTypeToChar aType)) $
      bs ^. aPods

spaceToPoint :: BurrowSpace -> Point
spaceToPoint (CorridorSpace i) = V2 (fromInteger i) 1
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

testBurrow :: BurrowState
testBurrow = MkBS burrow newPods
  where
    (MkBS burrow pods) = initBurrowState
    newPods =
      filter (\(MkApod rm@(Room raType num) aType) -> rm /= (Room A 1)) pods
