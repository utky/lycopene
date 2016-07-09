{-# LANGUAGE Rank2Types #-}
module Lycopene.Lens where

import Control.Applicative (Const(..), getConst)
import Control.Monad.Identity (Identity(..), runIdentity)

type Lens a b = forall f. Functor f => (b -> f b) -> (a -> f a)

-- TODO: make smarter
field :: (a -> b) -> (b -> a -> a) -> Lens a b
field getter setter f a = fmap ((flip setter) a) (f (getter a))

get :: Lens a b -> a -> b
get l = getConst . l Const

over :: Lens a b -> (b -> b) -> a -> a
over l m = runIdentity . l (Identity . m)

set :: Lens a b -> b -> a -> a
set l b = over l (const b)

