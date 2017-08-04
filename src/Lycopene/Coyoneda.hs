{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Lycopene.Coyoneda where

data Coyoneda f a where
    Coyoneda :: (b -> a) -> f b -> Coyoneda f a

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g v) = Coyoneda (f . g) v

hoistCoyoneda :: (forall x. f x -> g x) -> Coyoneda f a -> Coyoneda g a
hoistCoyoneda f (Coyoneda k m) = Coyoneda k (f m)
