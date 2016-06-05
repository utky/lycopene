{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lycopene.Core.Database.Type where

import           Control.Monad.Free (Free, liftF)


-- | `a` can be stored in some persistent system with key `k`
data Entity a k

instance (Eq k) => Eq (Entity a k) where
  x == y = (index x) == (index y)

index :: (Eq k) => Entity a k -> k
index = undefined

data Repository a


-- | Indicates data type `a` will be persisted in an appropriate storage.
newtype Persist a = Persist { unPersist :: a }

instance Functor Persist where
  fmap f = Persist . f . unPersist

instance Applicative Persist where
  pure = Persist
  f <*> a = fmap (unPersist f) a

instance Monad Persist where
  return = pure
  m >>= f = fmap (unPersist . f) m

persist :: a -> Persist a
persist = return

-- | Value from storage.
data Store a

-- FIXME: 関数として表現するのはダメそうどこか外からGivenにできるという
-- 仮定を表すべきなきがする　
-- p Parameter for fetch implementation
-- r Result of fetching
--

-- これはパラメータpを合成できないのでApplicative厳しそう
{-| 
    f Query method syntax
    a Return value
-}
type Fetch = Free

-- | 
runFetch :: (f a -> m a) -> Fetch f a -> m a
runFetch η fa = induce η fa

fetch :: f a -> Fetch f a
fetch = liftF
