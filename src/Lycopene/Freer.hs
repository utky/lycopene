{-# LANGUAGE RankNTypes #-}
module Lycopene.Freer where

import           Control.Monad.Free (Free, foldFree, liftF, hoistFree)
import           Lycopene.Coyoneda (Coyoneda(..), hoistCoyoneda)

-- | a.k.a. Operational Monad
type Freer f = Free (Coyoneda f)

liftR :: f a -> Freer f a
liftR = liftF . Coyoneda id

-- | Fold Freer with specified natural transformation.
foldFreer :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a 
foldFreer f = foldFree deCoyoneda where
  deCoyoneda (Coyoneda g b) = fmap g (f b)

hoistFreer :: (forall x. f x -> g x) -> Freer f a -> Freer g a
hoistFreer f = hoistFree (hoistCoyoneda f)
