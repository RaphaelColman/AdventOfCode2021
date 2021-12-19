module Solutions.Day18 where

import           Common.AoCSolutions (AoCSolution (MkAoCSolution),
                                      printSolutions, printTestSolutions)
import           Common.EnumUtils    (enumNext)
import           Control.Applicative ((<|>))
import           Control.Monad.Loops (iterateUntilM)
import           Data.Foldable       (foldlM)
import           Data.Function
import           Data.List           (findIndex)
import           Data.Maybe          (fromJust)
import           Debug.Trace
import           Text.Trifecta       (CharParsing (char), Parser, brackets,
                                      integer, parens)

aoc18 :: IO ()
aoc18 = do
  printTestSolutions 18 $ MkAoCSolution parseInput part1
  --printSolutions 18 $ MkAoCSolution parseInput part2

parseInput :: Parser Tree
parseInput = parsePair

parsePair :: Parser Tree
parsePair = do
  brackets $ do
    first <- parsePairElement
    char ','
    Pair first <$> parsePairElement
  where
    parsePairElement = Leaf <$> integer <|> parsePair

--part1 :: Tree -> Tree
part1 tree = toFirstExplodable tree >>= neighbour LEFT

part2 = undefined

data Tree
  = Pair Tree Tree
  | Leaf Integer
  deriving (Eq, Show)

data Direction
  = LEFT
  | RIGHT
  deriving (Eq, Show, Enum, Bounded)

type Breadcrumbs = [Crumb]

data Crumb =
  Crumb
    { _direction :: Direction
    , _tree      :: Tree
    }
  deriving (Eq, Show)

type Zipper = (Tree, Breadcrumbs)

zipDown :: Direction -> Zipper -> Maybe Zipper
zipDown direction (Pair l r, bs) =
  case direction of
    LEFT  -> Just (l, Crumb LEFT r : bs)
    RIGHT -> Just (r, Crumb RIGHT l : bs)
zipDown _ (Leaf _, _) = Nothing

zipUp :: Zipper -> Maybe Zipper
zipUp (tree, bc:rest) =
  case bc of
    Crumb LEFT subTree  -> Just (Pair tree subTree, rest)
    Crumb RIGHT subTree -> Just (Pair subTree tree, rest)
zipUp (_, []) = Nothing

zipToTop :: Zipper -> Zipper
zipToTop zipper@(tree, [])      = zipper
zipToTop zipper@(tree, bc:rest) = fromJust $ zipToTop <$> zipUp zipper

neighbour :: Direction -> Zipper -> Maybe Zipper
neighbour direction zipper@(tree, bc) =
  iterateUntilM
    (\z -> previousDirection z == Just oppositeDirection)
    zipUp
    zipper >>=
  zipUp >>=
  zipDown direction >>=
  iterateUntilM isLeaf (zipDown oppositeDirection)
  where
    oppositeDirection = enumNext direction

isLeaf :: Zipper -> Bool
isLeaf (Leaf i, _) = True
isLeaf _           = False

previousDirection :: Zipper -> Maybe Direction
previousDirection (_, (Crumb direction _):_) = Just direction
previousDirection _                          = Nothing

modify :: Tree -> Zipper -> Zipper
modify newValue (tree, bs) = (newValue, bs)

modifyAt :: Tree -> Tree -> [Direction] -> Maybe Tree
modifyAt newValue tree directions = do
  atSubTree <- foldlM (flip zipDown) asZipper directions
  pure $ fst $ modify newValue atSubTree & zipToTop
  where
    asZipper = (tree, [])

toFirstExplodable :: Tree -> Maybe Zipper
toFirstExplodable tree = go 0 (tree, [])
  where
    go :: Integer -> Zipper -> Maybe Zipper
    go depth zipper@(tree, bs)
      | depth == 4 =
        case tree of
          Leaf _   -> Nothing
          Pair l r -> Just zipper
      | otherwise = do
        let leftBranch = zipDown LEFT zipper >>= go (depth + 1)
        let rightBranch = zipDown RIGHT zipper >>= go (depth + 1)
        leftBranch <|> rightBranch

explode :: Zipper -> Tree
explode zipper@(Pair l r, bs) = undefined
  where
    lNeighbour = neighbour LEFT zipper
    rNeighbour = neighbour RIGHT zipper

extractDirections :: Zipper -> [Direction]
extractDirections (_, bs) = map _direction bs
