module Common.Predicates where

allPred :: Foldable f => f (a -> Bool) -> a -> Bool
allPred fs x = foldr (\f b -> b && f x) True fs

anyPred :: Foldable f => f (a -> Bool) -> a -> Bool
anyPred fs x = foldr (\f b -> b || f x) False fs

nonePred :: Foldable f => f (a -> Bool) -> a -> Bool
nonePred fs x = foldr (\f b -> b && not (f x)) True fs