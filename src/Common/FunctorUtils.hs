module Common.FunctorUtils where

fmap3 ::
     (Functor f1, Functor f2, Functor f3)
  => (a -> b)
  -> f1 (f2 (f3 a))
  -> f1 (f2 (f3 b))
fmap3 = fmap . fmap . fmap