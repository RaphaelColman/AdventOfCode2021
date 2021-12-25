module Solutions.Day18
  ( aoc18
  ) where

import           Combinatorics                  (variate)
import           Common.AoCSolutions            (AoCSolution (MkAoCSolution),
                                                 printSolutions,
                                                 printTestSolutions)
import           Common.EnumUtils               (enumNext)
import           Control.Applicative            ((<|>))
import           Control.Monad.Loops            (iterateUntilM)
import           Control.Monad.Trans.State.Lazy (State, StateT (StateT), get,
                                                 modify, put, runState)
import           Data.Foldable                  (foldlM)
import           Data.Function                  ((&))
import           Data.List                      (findIndex, foldl1', unfoldr)
import           Data.Maybe                     (fromJust, fromMaybe)
import           Text.Trifecta                  (CharParsing (char), Parser,
                                                 brackets, integer, parens,
                                                 some, token)
import Control.Monad.Cont (MonadTrans(lift), guard)

aoc18 :: IO ()
aoc18 = do
  printSolutions 18 $ MkAoCSolution parseInput part1
  printSolutions 18 $ MkAoCSolution parseInput part2

parseInput :: Parser [Tree]
parseInput = some $ token parsePair

parsePair :: Parser Tree
parsePair = do
  brackets $ do
    first <- parsePairElement
    char ','
    Pair first <$> parsePairElement
  where
    parsePairElement = Leaf <$> integer <|> parsePair

part1 :: [Tree] -> Integer
part1 = magnitude . sumTrees

part2 :: [Tree] -> Integer
part2 = maximum . map (magnitude . sumTrees) . variate 2

sumTrees :: [Tree] -> Tree
sumTrees = foldl1' go
  where
    go tree1 tree2 = simplify $ tree1 `add` tree2

data Tree
  = Pair Tree Tree
  | Leaf Integer
  deriving (Eq)

instance Show Tree where
  show (Leaf i)   = show i
  show (Pair l r) = show [l, r]

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

simplify :: Tree -> Tree
simplify tree =
  case explode tree of
    Just t  -> simplify t
    Nothing -> maybe tree simplify (split tree)

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

zipToTop :: Zipper -> Tree
zipToTop zipper@(tree, [])      = tree
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

modifyZipper :: Tree -> Zipper -> Zipper
modifyZipper newValue (tree, bs) = (newValue, bs)

addAtLeaf :: Integer -> [Direction] -> Tree -> Maybe Tree
addAtLeaf toAdd directions tree = do
  zipper@(atSubTree, bs) <- foldlM (flip zipDown) asZipper directions
  case atSubTree of
    Leaf i   -> pure $ modifyZipper (Leaf (i + toAdd)) zipper & zipToTop
    Pair _ _ -> Nothing
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

explode :: Tree -> Maybe Tree
explode tree = do
  zipper@(Pair (Leaf l) (Leaf r), bs) <- toFirstExplodable tree
  let with0 = zipper & modifyZipper (Leaf 0) & zipToTop
  let leftCarry =
        fromMaybe with0 $
        neighbour LEFT zipper >>= carry LEFT l with0 . extractDirections
  let rightCarry =
        fromMaybe leftCarry $
        neighbour RIGHT zipper >>= carry LEFT r leftCarry . extractDirections
  pure rightCarry
  where
    carry :: Direction -> Integer -> Tree -> [Direction] -> Maybe Tree
    carry direction value tree' directions = addAtLeaf value directions tree'

extractDirections :: Zipper -> [Direction]
extractDirections (_, bs) = reverse $ map _direction bs

toFirstSplittable :: Tree -> Maybe Zipper
toFirstSplittable tree = go (tree, [])
  where
    go :: Zipper -> Maybe Zipper
    go zipper@(Leaf i, bs)
      | i >= 10 = Just zipper
      | otherwise = Nothing
    go zipper@(tree, bs) = do
      let leftBranch = zipDown LEFT zipper >>= go
      let rightBranch = zipDown RIGHT zipper >>= go
      leftBranch <|> rightBranch

split :: Tree -> Maybe Tree
split tree = do
  zipper@(Leaf l, bs) <- toFirstSplittable tree
  let (lft, rt) = splitInteger l
  pure $ modifyZipper (Pair (Leaf lft) (Leaf rt)) zipper & zipToTop

splitInteger :: Integer -> (Integer, Integer)
splitInteger i =
  let divided = i `div` 2
   in (divided, i - divided)

add :: Tree -> Tree -> Tree
add = Pair

magnitude :: Tree -> Integer
magnitude = go
  where
    go (Leaf i)     = i
    go (Pair p1 p2) = (3 * go p1) + (2 * go p2)
