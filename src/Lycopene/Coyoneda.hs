{-# LANGUAGE GADTs #-}
module Lycopene.Coyoneda where

data Coyoneda f a where
    Coyoneda :: (b -> a) -> f b -> Coyoneda f a	 

instance Functor (Coyoneda f) where
  fmap f (Coyoneda g v) = Coyoneda (f . g) v

