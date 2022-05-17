module Common.GraphUtils where

import qualified Data.Set as S
import qualified Data.PSQueue as PQ
import qualified Data.Map as M

class (Eq a, Ord a) => WGraphNode a where
  neighbours :: a -> S.Set (a, Integer)

dijkstra :: (Eq a, Ord a) => a -> (a -> M.Map a Integer) -> (a -> Bool) -> Maybe Integer
dijkstra startingNode neighbourNodes isDestination = go (PQ.singleton startingNode 0) M.empty S.empty
  where
    go pq costs visited = do
      (current PQ.:-> cost, remainingQueue) <- PQ.minView pq
      let isLowerCost = maybe True (> cost) $ M.lookup current costs
      if isDestination current
        then pure cost
        else
          if current `S.notMember` visited && isLowerCost
            then
              let newVisited = S.insert current visited
                  newCosts = M.insert current cost costs
                  unvisitedChildren =
                    M.filterWithKey
                      (\node cost -> node `S.notMember` visited) $
                    neighbourNodes current
                  newPQ =
                    M.foldrWithKey
                      (\node' cost' pq' ->
                         PQ.insertWith min node' (cost' + cost) pq')
                      pq
                      unvisitedChildren
              in ($!) go newPQ newCosts newVisited
            else ($!) go remainingQueue costs visited

