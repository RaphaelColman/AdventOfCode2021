{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day23 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions)
import           Common.Debugging
import           Common.Geometry     (Point, renderVectorMap)
import           Common.ListUtils    (flexibleRange, singleton, window3)
import           Common.MapUtils     (minimumValue)
import qualified Common.SetUtils     as SU
import           Control.Lens
import           Data.Foldable       (find, minimumBy, traverse_)
import           Data.Function       (on)
import qualified Data.Map            as M
import           Data.Maybe          (isJust, isNothing, mapMaybe)
import qualified Data.Set            as S
import           Debug.Trace
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

type BurrowState = S.Set Amphipod

data Move =
  MkMove
    { _state :: BurrowState
    , _cost  :: Integer
    }
  deriving (Eq, Show, Ord)

makeLenses ''Amphipod

aoc23 :: IO ()
aoc23
  --let nexts = S.toList $ nextMoves initBurrowState
  --traverse_ debugLn nexts
  --print "number of next moves"
  --print $ length nexts
  --let nextNode = minimumBy (compare `on` _cost) nexts
  --print "Lowest cost node:"
  --debugLn nextNode
 = do
  printSolutions 23 $ MkAoCSolution parseInput part1
  --printSolutions 23 $ MkAoCSolution parseInput part2
  where
    debugLn (MkMove state cost) = do
      putStrLn $ renderVectorMap $ renderBurrowState state
      print cost

testBurrow :: BurrowState
testBurrow = S.fromList amphipods
  where
    amphipods = zipWith MkApod rooms podTypes
    podTypes = [B, A, A, B, C, C, D, D]
    rooms = zipWith Room [A, A, B, B, C, C, D, D] $ cycle [1, 2]

parseInput :: Parser String
parseInput = pure "unimplemented"

--part1 :: String -> String
part1 str = dijkstra initBurrowState

part2 :: String -> String
part2 = undefined

movementCost :: AmType -> Integer
movementCost aType =
  case aType of
    A -> 1
    B -> 10
    C -> 100
    D -> 1000

initBurrowState :: BurrowState
initBurrowState = S.fromList amphipods
  where
    amphipods = zipWith MkApod rooms podTypes
    --podTypes = [B, A, C, D, B, C, D, A]
    podTypes = [D, D, A, C, C, B, A, B]
    rooms = zipWith Room [A, A, B, B, C, C, D, D] $ cycle [1, 2]

burrowComplete :: BurrowState -> Bool
burrowComplete bs = all (`isFinished` bs) bs

isFinished :: Amphipod -> BurrowState -> Bool
isFinished (MkApod position amType) bs
  | position == Room amType 2 = True -- no need to move if you're in the deepest room of your type
  | position == Room amType 1 =
    case getOccupant (Room amType 2) bs of
      Just a  -> _amType a == amType --Finished if you're in room 1 and room 2 is occupied by an amphipod of the correct type
      Nothing -> False
  | otherwise = False

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
  | null destinationOccupants = Just $ Room aType 2
  | length destinationOccupants == 1 = Just $ Room aType 1
  | length destinationOccupants == 2 = Nothing --This room is already complete
  | otherwise = error "More than two occupants for only two rooms"
  where
    destinationOccupants = mapMaybe ((`getOccupant` bs) . Room aType) [1, 2]

--Maybe this would be quicker if I enumerated all the different moves individually for each amphipod (rather than dividing them into room and corridor moves)
--Maybe also quicker if I rule out 'blocked amphipods'
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

--Slows down well before it will get to the solution. Might be from comparing all those sets?
dijkstra :: BurrowState -> Integer
dijkstra bs = go bs (M.fromList [(bs, 0)]) S.empty
  where
    go current tDistances visited
      | burrowComplete current = tDistances M.! current
      | otherwise = ($!) go minNode newTDistances newVisited
      where
        unvisitedChildren =
          S.filter (\(MkMove state cost) -> not (state `S.member` visited)) $
          nextMoves current
        distances =
          M.mapKeys _state $
          M.fromSet
            (\(MkMove state cost) -> cost + tDistances M.! current)
            unvisitedChildren
        newTDistances = M.unionWith min distances tDistances
        (minNode, minEnergy) =
          minimumValue $
          M.filterWithKey (\k v -> k `S.notMember` newVisited) newTDistances --According the profile, this is the most expensive bit.  Could it be that `minimumValue`  turns it into a list of tuples?
        newVisited = S.insert current visited
--The trace is defo centered around the S.notMember bit, so not sure it's to do with minimumValue

--Route from start node to end node (including start and end ndoes)
--This probably does not have to be in order and can be simplified
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
